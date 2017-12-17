{ config, pkgs, ... }:

{
  imports = [
    ./desktop-light.nix
  ];

  environment.systemPackages = with pkgs; [
    firefox
    slack
    spotify
    #skype
  ];
}
