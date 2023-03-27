{pkgs ? import <nixpkgs>}:
#with import <nixpkgs> {};

pkgs.writeShellApplication { 
  name = "pipewire-vol";

  runtimeInputs = with pkgs; [
    pamixer
  ];

  text = builtins.readFile ./pipewire-vol;
}

