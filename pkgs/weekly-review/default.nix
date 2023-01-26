{pkgs ? import <nixpkgs>}:
#with import <nixpkgs> {};

pkgs.writeShellApplication {
  name = "weekly-review"; 
  text = builtins.readFile ./weekly-review;
}

