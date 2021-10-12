{ pkgs, ... }:

{
  home.file = {
    ".config/khal/config".source = ./dot-khal;
  };

  home.packages = with pkgs; [
    khal
  ];
}
