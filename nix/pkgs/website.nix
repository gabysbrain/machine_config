{pkgs ? import <nixpkgs>}:
#with import <nixpkgs> {};

let src = pkgs.fetchFromGitHub {
    owner  = "gabysbrain";
    repo   = "website";
    rev    = "master";
    sha256 = "05lhngb9zahxlbns1rf7s01n48sbm98bdhv6vkdwl6fkd6pql06q";
  };
in
import "${src}/default.nix"
