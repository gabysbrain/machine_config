{ pkgs, ... }:

{
  home.file = {
    ".config/mimeo/associations.txt".source = ./dot-associations;
  };

  home.packages = with pkgs; [
    mimeo
  ];
}
