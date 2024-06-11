{
  description = "Tinderbox expression language";

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
      flake = pkgs.tinderbox.flake {
      };
      overlays = [ haskellNix.overlay
        (final: prev: {
          tinderbox =
            final.haskell-nix.project' {
              src = ./.;
              supportHpack = true;
              compiler-nix-name = "ghc96";
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
      packages.default = flake.packages."tinderbox:exe:tinderbox";
      devShell = flake.devShell // {
        withHoogle = true;
      };
    });
}
