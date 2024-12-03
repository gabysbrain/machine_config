{pkgs ? import <nixpkgs>}:
#with import <nixpkgs> {};

pkgs.writeShellApplication {
  name = "syncmail";
  runtimeInputs = with pkgs; [ lieer (callPackage ../notmuch-tag {}) ];
  text = builtins.readFile ./syncmail;
}

