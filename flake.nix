{
  description = "Trade journal software";

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
      flake = pkgs.org-data.flake {
      };
      overlays = [ haskellNix.overlay
        (final: prev: {
          org-data =
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
            };
        })
      ];
    in {
      packages.default = flake.packages."org-data:exe:org-data";
      devShell = flake.devShell // {
        withHoogle = true;
      };
    });
}
