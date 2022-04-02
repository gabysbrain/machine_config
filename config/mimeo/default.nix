{ pkgs, ... }:

let 
  xdg-open = pkgs.writeShellApplication {
    name = "xdg-open";
    runtimeInputs = with pkgs; [ mimeo ];
    text = ''
      mimeo "$1"
    '';
  };
  xdg-email = pkgs.writeShellApplication {
    name = "xdg-email";
    runtimeInputs = with pkgs; [ mimeo ];
    text = ''
      mimeo "$1"
    '';
  };
  xdg-mime = pkgs.writeShellApplication {
    name = "xdg-mime";
    runtimeInputs = with pkgs; [ mimeo ];
    text = ''
      mimeo -m "$1"
    '';
  };
in
{
  home.file = {
    ".config/mimeo/associations.txt".source = ./dot-associations;
  };

  home.packages = with pkgs; [
    mimeo

    # patched xdg-utils
    xdg-open
    xdg-mime
  ];
}
