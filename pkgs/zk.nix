{ pkgs }:
let src = pkgs.fetchFromGitHub {
    owner  = "gabysbrain";
    repo   = "zk";
    rev    = "be2563a46500bb078a68b62e6903511c59016319";
    sha256 = "12fh8vp8bjy149bwf0xg35xq19sf4bqvvwpnqq2xin2wwmzw02h3";
  };
in
pkgs.haskellPackages.callPackage "${src}/default.nix" {}
