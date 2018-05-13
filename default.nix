{ pkgs ? import <nixpkgs> {} }:
  pkgs.haskellPackages.callPackage ./waffle.nix {}

