{pkgs ? import <nixpkgs>}:
#with import <nixpkgs> {};

pkgs.writeShellApplication {
  name = "summ_paper";

  runtimeInputs = with pkgs; [ mimeo ];

  text = builtins.readFile ./summ_paper;
}

