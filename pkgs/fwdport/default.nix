{pkgs ? import <nixpkgs>}:
#with import <nixpkgs> {};

pkgs.writeShellApplication {
  name = "fwdport";

  runtimeInputs = with pkgs; [ ];

  text = builtins.readFile ./fwdport;
}
