{pkgs ? import <nixpkgs>, juliaPkg }:
#with import <nixpkgs> {};

pkgs.runCommand "jl" {
  buildInputs = with pkgs; [ tmux juliaPkg ];
} ''
  mkdir -p $out/bin
  cp ${./jl} $out/bin/jl
  sed -i "2 i export PATH=$PATH" $out/bin/jl
''

