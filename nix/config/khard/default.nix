{ pkgs, ... }:

{
  home.file = {
    ".config/khard/khard.conf".source = ./dot-khard;
  };

  home.packages = with pkgs; [
    khard
  ];
}
