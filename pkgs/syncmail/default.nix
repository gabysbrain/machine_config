{pkgs ? import <nixpkgs>}:
#with import <nixpkgs> {};

pkgs.writeShellApplication {
  name = "syncmail";
  runtimeInputs = with pkgs; [ gmailieer (callPackage ../notmuch-tag {}) ];
  text = builtins.readFile ./syncmail;
}

