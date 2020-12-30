# Run using:
#
#     $(nix-build --no-link -A fullBuildScript)
{
  pkgName,
  projectDir ? toString "./.",
  compiler ? "ghc865", # matching stack.yaml
  stack2nix-output-path ? "./rapid-output.nix"
}:
let
  cabalPackageName = pkgName;

  # Pin static-haskell-nix version.
  static-haskell-nix =
    if builtins.pathExists ../.in-static-haskell-nix
      then toString ../. # for the case that we're in static-haskell-nix itself, so that CI always builds the latest version.
      # Update this hash to use a different `static-haskell-nix` version:
      else fetchTarball https://github.com/nh2/static-haskell-nix/archive/d1b20f35ec7d3761e59bd323bbe0cca23b3dfc82.tar.gz;

  # Pin nixpkgs version
  # By default to the one `static-haskell-nix` provides, but you may also give
  # your own as long as it has the necessary patches, using e.g.
  #     pkgs = import (fetchTarball https://github.com/nh2/nixpkgs/archive/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa123.tar.gz) {};
  pkgs = import "${static-haskell-nix}/nixpkgs.nix";

  stack2nix-script = import "${static-haskell-nix}/static-stack2nix-builder/stack2nix-script.nix" {
    inherit pkgs;
    stack-project-dir = projectDir; # where stack.yaml is
    hackageSnapshot = "2020-02-15T00:00:00Z"; # pins e.g. extra-deps without hashes or revisions
  };

  static-stack2nix-builder = import "${static-haskell-nix}/static-stack2nix-builder/default.nix" {
    normalPkgs = pkgs;
    inherit cabalPackageName compiler stack2nix-output-path;
    # disableOptimization = true; # for compile speed
  };

  # Full invocation, including pinning `nix` version itself.
  fullBuildScript = pkgs.writeShellScript "stack2nix-and-build-script.sh" ''
    set -eu -o pipefail
    STACK2NIX_OUTPUT_PATH=$(${stack2nix-script})
    export NIX_PATH=nixpkgs=${pkgs.path}
    ${pkgs.nix}/bin/nix-build --no-link -A static_package --argstr stack2nix-output-path "$STACK2NIX_OUTPUT_PATH" "$@"
  '';
  # nixpkgs containing all of static-haskell-nix's overrides.
  static_pkgs = static-stack2nix-builder.haskell-static-nix_output.pkgs;

  # Haskell package set, containing our packages with static overrides.
  static_haskellPackages = static-stack2nix-builder.haskell-static-nix_output.haskellPackages;

  # Adds override necessary for postgres libraries, see https://github.com/nh2/static-haskell-nix/issues/57
  addPostgresFlags = drv:
    static_pkgs.staticHaskellHelpers.addStaticLinkerFlagsWithPkgconfig
      drv
      (with static_pkgs; [ openssl postgresql ])
      "--libs libpq";

  # Exclude `.git`, `.stack-work` and other big directories from a Haskell
  # packages's `src`.
  # Contains extra functionality for tracing which files are let through.
  setCleanHaskellSrc = drv:
    with pkgs.haskell.lib; with pkgs.lib; overrideCabal drv (old: {
      src =
        let
          cleanedSrc = cleanSourceWith {
            src = old.src;
            filter = fullPath: type:
              let
                baseName = baseNameOf (toString fullPath);
                includeInInSrcBool = all (b: b) [
                  (cleanSourceFilter fullPath type) # remove version control dirs etc.
                  (!(hasPrefix "." baseName)) # remove dotfiles/dirs
                  (!(hasSuffix ".nix" baseName)) # remove .nix files
                  (!(baseName == "build")) # remove generated files
                ];
              in includeInInSrcBool;
          };
        in
        builtins.trace "Using cleaned source dir for ${old.pname}: ${cleanedSrc}" cleanedSrc;
    });

  # Fixes we want to make to any Haskell packages go in here.
  static_haskellPackages_with_fixes = static_haskellPackages.override (old: {
    overrides = pkgs.lib.composeExtensions (old.overrides or (_: _: {})) (self: super: {

      rapid = setCleanHaskellSrc super.rapid;
      rapid-example = setCleanHaskellSrc "./example";

    });
  });

  static_package = static_haskellPackages_with_fixes.rapid-example;

  # Generates `./build/function.zip` and `./swagger.json` in the local directory.
  lambda_function_zip_script = pkgs.writeShellScript "generate-lamda-function-zip.sh" ''
    set -eu -o pipefail
    STATIC_BUILD_OUT_PATH=$(${fullBuildScript})
    $STATIC_BUILD_OUT_PATH/bin/generate-swagger
    rm -rf build/
    (
      set -e;
      mkdir build && cd build
      cp $STATIC_BUILD_OUT_PATH/bin/bootstrap .
      zip -r function.zip ./bootstrap data/
    )
  '';

in
  {
    static_package = static-stack2nix-builder.static_package;
    inherit fullBuildScript;
    inherit lambda_function_zip_script;
    # For debugging:
    inherit stack2nix-script;
    inherit static-stack2nix-builder;
  }
