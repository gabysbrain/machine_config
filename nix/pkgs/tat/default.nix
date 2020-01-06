{pkgs ? import <nixpkgs>}:
#with import <nixpkgs> {};

pkgs.runCommand "tat" {
  buildInputs = with pkgs; [ tmux ];
} ''
  mkdir -p $out/bin
  cp ${./tat} $out/bin/tat
  sed -i "2 i export PATH=$PATH" $out/bin/tat
''

