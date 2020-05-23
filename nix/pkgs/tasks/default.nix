{pkgs ? import <nixpkgs>}:
#with import <nixpkgs> {};

pkgs.runCommand "tasks" {
  buildInputs = with pkgs; [ tmux ];
} ''
  mkdir -p $out/bin
  cp ${./tasks} $out/bin/tasks
  #sed -i "2 i export PATH=$PATH" $out/bin/tasks
''

