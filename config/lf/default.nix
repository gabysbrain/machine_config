{ pkgs, ... }:

{
  home.file = {
    ".config/lf/lfrc".source = ./dot-lfrc;
  };

  home.packages = with pkgs; [
    lf
  ];
}
