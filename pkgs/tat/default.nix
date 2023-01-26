{pkgs ? import <nixpkgs>}:
#with import <nixpkgs> {};

pkgs.writeShellApplication {
  name = "tat"; 
  runtimeInputs = with pkgs; [ tmux ];
  text = builtins.readFile ./tat;
}

