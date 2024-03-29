{
  description = "calclulate block types needed for an assembler upgrade";

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
            dev.packages.${system}.default
            pkgs.cabal2nix
            pkgs.nix-prefetch-git
            pkgs.cabal-install
            pkgs.ghc
          ];
          shellHook = ''
          '';
        };
      });
}
