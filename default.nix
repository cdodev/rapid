{ system ? builtins.currentSystem
, crossSystem ? null
# allows to cutomize ghc and profiling (see ./nix/haskell.nix):
, config ? {}
# allows to override dependencies of the project without modifications,
# eg. to test build against local checkout of nixpkgs and iohk-nix:
# nix build -f default.nix iohk-skeleton --arg sourcesOverride '{
#   iohk-nix = ../iohk-nix;
#   nixpkgs  = ../nixpkgs;
# }'
, sourcesOverride ? {}
# pinned version of nixpkgs augmented with iohk overlays.
, pkgs ? import ./nix {
    inherit system crossSystem config sourcesOverride;
  }
}:
# commonLib include iohk-nix utilities, our util.nix and nixpkgs lib.
with pkgs; with commonLib;
pkgs.haskell-nix.stackProject {
  src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./.; };
  # pkg-def-extras = [
  #   # Additional packages ontop of all those listed in `stack.yaml`
  # ];
  # modules = [
  #   # Specific package overrides would go here for example:
  #   packages.cbors.package.ghcOptions = "-Werror";
  #   packages.cbors.patches = [ ./one.patch ];
  #   packages.cbors.flags.optimize-gmp = false;
  #   # It may be better to set flags in `stack.yaml` instead
  #   # (`stack-to-nix` will include them as defaults).
  # ];
}
