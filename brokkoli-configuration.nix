# This is actually a home-manager config file because the base is WSL Ubuntu
{ config, pkgs, ... }:

{
  imports = [
    ./home-config/common.nix

    ./profiles/dev.nix
    ./profiles/writing.nix
    ./profiles/vrvis.nix

    ./config/nonixos.nix
  ];

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "torsney-weir";
  home.homeDirectory = "/home/torsney-weir";

  home.packages = with pkgs; [
    # for reviewing papers
    (callPackage ./pkgs/summ_paper {})

    meld
    youtube-dl
  ];

  # nerdfonts needed for zsh prompt
  fonts.fontconfig.enable = true;
  home.packages = with pkgs; [
    nerdfonts
  ];

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.05";
}
