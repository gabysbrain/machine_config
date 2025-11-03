{pkgs ? import <nixpkgs>}:
#with import <nixpkgs> {};

pkgs.writeShellApplication { 
  name = "agg";

  runtimeInputs = with pkgs; [
    silver-searcher
    fzf
  ];

  text = builtins.readFile ./agg;
}

