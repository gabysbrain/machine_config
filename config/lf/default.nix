{ pkgs, ... }:

{
  home.file = {
    ".config/lf/lfrc".source = ./dot-lfrc;
  };

  home.programs = with pkgs; [
    lf
  ];
}
