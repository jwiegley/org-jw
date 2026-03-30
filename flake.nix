{
  description = "Org data tools";

  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    haskellNix.url = "github:input-output-hk/haskell.nix";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };
        overlays = [ haskellNix.overlay
          (final: prev: {
            org-jw =
              final.haskell-nix.project' {
                src = ./.;
                supportHpack = true;
                compiler-nix-name = "ghc910";
                shell = {
                  tools = {
                    cabal = {};
                    haskell-language-server = {};
                  };
                  buildInputs = with pkgs; [
                    pkg-config
                    haskellPackages.fourmolu
                    haskellPackages.hlint
                    haskellPackages.hpc
                    lefthook
                  ];
                  withHoogle = true;
                };
                # modules = [{
                #   enableLibraryProfiling = true;
                #   enableProfiling = true;
                # }];
              };
          })
        ];
        flake = pkgs.org-jw.flake {
        };

        # All Haskell source directories (excluding dist-newstyle)
        hsDirs = builtins.concatStringsSep " " [
          "flatparse-util"
          "org-cbor"
          "org-data"
          "org-db"
          "org-filetags"
          "org-json"
          "org-jw"
          "org-lint"
          "org-parse"
          "org-print"
          "org-site"
          "org-types"
        ];

      in flake // {
        packages.default = flake.packages."org-jw:exe:org";

        devShells.default = flake.devShells.default // {
          withHoogle = true;
        };

        checks = (flake.checks or {}) // {
          formatting = pkgs.runCommand "check-formatting" {
            nativeBuildInputs = [ pkgs.haskellPackages.fourmolu pkgs.findutils ];
            src = self;
          } ''
            cd $src
            find ${hsDirs} -name '*.hs' -type f \
              | xargs fourmolu --mode check
            touch $out
          '';

          hlint = pkgs.runCommand "check-hlint" {
            nativeBuildInputs = [ pkgs.haskellPackages.hlint pkgs.findutils ];
            src = self;
          } ''
            cd $src
            find ${hsDirs} -name '*.hs' -type f \
              | xargs hlint
            touch $out
          '';
        };
      });
}
