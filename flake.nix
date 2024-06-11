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
      flake = pkgs.org-jw.flake {
      };
      overlays = [ haskellNix.overlay
        (final: prev: {
          org-jw =
            final.haskell-nix.project' {
              src = ./.;
              supportHpack = true;
              compiler-nix-name = "ghc98";
              shell.tools = {
                cabal = {};
                haskell-language-server = {};
                hlint = {};
              };
              shell.buildInputs = with pkgs; [
                pkg-config
              ];
              modules = [{
                enableLibraryProfiling = false;
                enableProfiling = false;
              }];
            };
        })
      ];
    in flake // {
      packages.default = flake.packages."org-lint:exe:org-lint";

      devShell = flake.devShell // {
        packages = p: [
        ];

        buildInputs = with pkgs.haskellPackages; [
          cabal-install
          ghcid
        ];

        withHoogle = true;
      };
    });
}
