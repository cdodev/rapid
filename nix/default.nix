{ system ? builtins.currentSystem
, crossSystem ? null
, config ? {}
, sourcesOverride ? {}
}:
let
  haskellNix = import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz) {};
  ghcide = (import (builtins.fetchTarball "https://github.com/cachix/ghcide-nix/tarball/master") {}).ghcide-ghc865;
  all-hies = import (fetchTarball "https://github.com/infinisil/all-hies/tarball/master") {};
  # for inclusion in pkgs:
  overlays =
    # Haskell.nix (https://github.com/input-output-hk/haskell.nix)
    haskellNix.overlays
    ++ [
      (pkgs: _: with pkgs; {
        # commonLib: mix pkgs.lib with iohk-nix utils and our own:
        commonLib = lib // iohkNix
          // import ./util.nix { inherit haskell-nix; }
          # also expose our sources and overlays
          // { inherit overlays sources; };
        hie = (all-hies.selection { selector = p: { inherit (p) ghc865; }; });
        ghcide = ghcide;

      })
      # Our haskell-nix-ified cabal project:
      # (import ./pkgs.nix)
    ];

  pkgs = import haskellNix.sources.nixpkgs-1909 { inherit overlays config; };

in pkgs
