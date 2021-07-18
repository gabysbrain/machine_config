{pkgs ? import <nixpkgs>}:
#with import <nixpkgs> {};

pkgs.runCommand "weekly-review" {
  buildInputs = with pkgs; [ ];
} ''
  mkdir -p $out/bin
  cp ${./weekly-review} $out/bin/weekly-review
  #sed -i "2 i export PATH=$PATH" $out/bin/weekly-review
''

