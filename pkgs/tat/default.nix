{pkgs ? import <nixpkgs>}:
#with import <nixpkgs> {};

pkgs.writeShellApplication {
  name = "tat"; 
  runtimeInputs = with pkgs; [ tmux gnugrep ];
  text = builtins.readFile ./tat;
}

