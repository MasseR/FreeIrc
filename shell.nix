{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let
  # 87a5de01a7ed56330c8b360aa3ee00c70a248d59
  _nixpkgs = import (nixpkgs.pkgs.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "87a5de01a7ed56330c8b360aa3ee00c70a248d59";
    sha256 = "0plbxmwxvvz6xg26ymfrq0qgvja8zss0bn1n9cmfcgbsmk7j0vfx";
  }) {};

  inherit (_nixpkgs) pkgs;


  f = import ./default.nix;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
