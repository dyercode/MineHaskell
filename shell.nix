{ pkgs ? import <nixpkgs> {}}:
pkgs.mkShell {
  nativeBuildInputs = [
    pkgs.cabal2nix
    pkgs.nix-prefetch-git
    pkgs.cabal-install
    pkgs.ghc
  ];
  shellHook = ''
    # cabal update # as predicted, a bit agressive.
  '';
}
