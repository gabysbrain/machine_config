{pkgs ? import <nixpkgs>}:
#with import <nixpkgs> {};

pkgs.runCommand "summ_paper" {
  buildInputs = with pkgs; [ ];
} ''
  mkdir -p $out/bin
  cp ${./summ_paper} $out/bin/summ_paper
  #sed -i "2 i export PATH=$PATH" $out/bin/summ_paper
''

