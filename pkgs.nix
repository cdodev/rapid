{
  pkgs = hackage:
    {
      packages = {
        "ghc-prim".revision = (((hackage."ghc-prim")."0.5.3").revisions).default;
        "rts".revision = (((hackage."rts")."1.0").revisions).default;
        "base".revision = (((hackage."base")."4.12.0.0").revisions).default;
        "integer-gmp".revision = (((hackage."integer-gmp")."1.0.2.0").revisions).default;
        };
      compiler = {
        version = "8.6.5";
        nix-name = "ghc865";
        packages = {
          "ghc-prim" = "0.5.3";
          "rts" = "1.0";
          "base" = "4.12.0.0";
          "integer-gmp" = "1.0.2.0";
          };
        };
      };
  extras = hackage:
    { packages = { raid = ./.plan.nix/raid.nix; }; };
  modules = [ ({ lib, ... }: { packages = { "raid" = { flags = {}; }; }; }) ];
  }