{pkgs ? import <nixpkgs>}:
#with import <nixpkgs> {};

# like https://github.com/ThePrimeagen/.dotfiles/blob/master/bin/.local/scripts/tmux-sessionizer but adapted to me
let 
  tat = pkgs.callPackage ../tat {};
in
pkgs.writeShellApplication { 
  name = "tmux-projs";

  runtimeInputs = with pkgs; [
    tat
  ];

  text = builtins.readFile ./tmux-projs;
}

