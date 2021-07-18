{pkgs ? import <nixpkgs>}:
#with import <nixpkgs> {};

pkgs.runCommand "syncmail" {
  buildInputs = with pkgs; [ gmailieer ];
} ''
  mkdir -p $out/bin
  cp ${./syncmail} $out/bin/syncmail
  #sed -i "2 i export PATH=$PATH" $out/bin/syncmail
''

