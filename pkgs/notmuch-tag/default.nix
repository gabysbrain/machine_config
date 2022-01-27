{pkgs ? import <nixpkgs>}:
#with import <nixpkgs> {};

pkgs.runCommand "notmuch-tag" {
  buildInputs = with pkgs; [ notmuch ];
} ''
  mkdir -p $out/bin
  cp ${./notmuch-tag} $out/bin/notmuch-tag
  sed -i "2 i export PATH=$PATH" $out/bin/notmuch-tag
''

