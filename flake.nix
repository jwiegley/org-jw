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
      flake = pkgs.org-lint.flake {
      };
      overlays = [ haskellNix.overlay
        (final: prev: {
          org-types =
            final.haskell-nix.project' {
              src = ./org-types;
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
          org-data =
            final.haskell-nix.project' {
              src = ./org-data;
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
          org-parse =
            final.haskell-nix.project' {
              src = ./org-parse;
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
          org-print =
            final.haskell-nix.project' {
              src = ./org-print;
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
          org-lint =
            final.haskell-nix.project' {
              src = ./org-lint;
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
          filetags =
            final.haskell-nix.project' {
              src = ./filetags;
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
        ];

        withHoogle = true;
      };
    });
}
