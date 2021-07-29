{pkgs ? import <nixpkgs>}:
#with import <nixpkgs> {};

pkgs.runCommand "archive_mails" {
  buildInputs = with pkgs; [ notmuch ];
} ''
  mkdir -p $out/bin
  cp ${./archive_mails} $out/bin/archive_mails
''

