#{pkgs ? import <nixpkgs>}:
with import <nixpkgs> {};

let
  confPath = pkgs.copyPathToStore ./gcal.conky;
in
pkgs.writeShellScriptBin "gcal-conky" ''
  ${pkgs.conky}/bin/conky -c ${confPath}
  ''

