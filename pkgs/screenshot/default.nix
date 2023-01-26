{pkgs ? import <nixpkgs>}:
#with import <nixpkgs> {};

pkgs.writeShellApplication {
  name = "screenshot";

  runtimeInputs = with pkgs; [ shutter dmenu ];

  text = builtins.readFile ./screenshot;
}
