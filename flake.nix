{
  description = "Achievement sersr";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    dev.url = "github:/dyercode/dev";
  };

  outputs = { self, nixpkgs, flake-utils, dev }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
      in {
        devShells.default = pkgs.mkShell {
          nativeBuildInputs = [
            dev.defaultPackage.${system}
            pkgs.cabal2nix
            pkgs.nix-prefetch-git
            pkgs.cabal-install
            pkgs.ghc
          ];
          shellHook = ''
            # cabal update # as predicted, a bit agressive.
          '';
        };
      });
}
