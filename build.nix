{
  sources ? import ./nix/sources.nix,
  stack2nix-output-path ? ./rapid-output.nix,
  projectDir ? ./.
}:
# with import sources.nixpkgs {};
let
  # rapid = import sources.rapid {
  # Use local checkout of rapid for faster iteration
  rapid = import ./. {
            pkgName = "rapid";
            compiler = "ghc865";
            inherit stack2nix-output-path projectDir;
          };

in
  rapid.fullBuildScript
