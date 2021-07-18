{ pkgs, ... }:

{
  home.file = {
    ".vdirsyncer/config".source = ./dot-vdirsyncer;
  };

  home.packages = with pkgs; [
    isync
  ];
}
