let
  hsPkgs = import ./default.nix { };
  pkgs = import ./nix {};
in
hsPkgs.shellFor {
      name = "rapid-shell"
      packages = ps: [ps.rapid];
      withHoogle = true;
      buildInputs = with pkgs.haskellPackages; with pkgs;
       [ hlint stylish-haskell ghcid ghcide cabal-install hie haskell-nix.nix-tools ];

      shellHook = ''
        # check if it's still needed ?
        # https://github.com/NixOS/nixpkgs/issues/55539
        export HIE_HOOGLE_DATABASE="$(cat $(${pkgs.which}/bin/which hoogle) | sed -n -e 's|.*--database \(.*\.hoo\).*|\1|p')"
      '';
  }
