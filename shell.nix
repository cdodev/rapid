let
  hsPkgs = import ./default.nix { };
  pkgs = import ./nix {};
in
hsPkgs.shellFor {
      packages = ps: [ps.raid];
      withHoogle = true;
      buildInputs = with pkgs.haskellPackages; with pkgs;
       [ hlint stylish-haskell ghcid ghcide cabal-install hie haskell-nix.nix-tools ];

      shellHook = ''
        # check if it's still needed ?
        # https://github.com/NixOS/nixpkgs/issues/55539
        export HIE_HOOGLE_DATABASE="$NIX_GHC_LIBDIR/../../share/doc/hoogle/index.html"
        source ./run_daemon


      '';
  }
