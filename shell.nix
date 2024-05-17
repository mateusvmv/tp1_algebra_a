{ pkgs ? import <nixpkgs> {} }:
with pkgs; mkShell {
  packages = [
    (haskell.packages.ghc948.ghcWithPackages (pkgs: with pkgs; [
      unordered-containers
      random
    ]))
  ];
}