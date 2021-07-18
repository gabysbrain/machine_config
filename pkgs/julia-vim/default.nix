{pkgs ? import <nixpkgs>}:
#with import <nixpkgs> {};

pkgs.runCommand "julia-vim" {
  buildInputs = with pkgs; [ tmux ];
} ''
  mkdir -p $out/bin
  cp ${./julia-vim} $out/bin/julia-vim
  #sed -i "2 i export PATH=$PATH" $out/bin/julia-vim
''

