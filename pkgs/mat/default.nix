{pkgs ? import <nixpkgs>}:
#with import <nixpkgs> {};

pkgs.runCommand "mat" {
  buildInputs = with pkgs; [ tmux ];
} ''
  mkdir -p $out/bin
  cp ${./mat} $out/bin/mat
  #sed -i "2 i export PATH=$PATH" $out/bin/mat
''

