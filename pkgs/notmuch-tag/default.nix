{pkgs ? import <nixpkgs>}:
#with import <nixpkgs> {};

pkgs.writeShellApplication {
  name = "notmuch-tag"; 

  runtimeInputs = with pkgs; [ notmuch ];

  text = builtins.readFile ./notmuch-tag;
}

