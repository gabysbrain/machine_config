{ config, pkgs, ... }:

{
  hardware.pulseaudio = {
    enable = true;
  };
  nixpkgs.config.pulseaudio = true;

  # see https://nixos.wiki/wiki/Steam
  # needed for steam 32 bit
  hardware.opengl.driSupport32Bit = true;
  hardware.opengl.extraPackages32 = with pkgs.pkgsi686Linux; [ libva ];
  hardware.pulseaudio.support32Bit = true;

  programs.steam.enable = true;

  environment.systemPackages = with pkgs; [
    discord
    mumble

    # steam comes from the above programs.steam option
    vassal # mmm board games
    lutris # windows games

    nethack
    wesnoth
    zeroad # 0 a.d.
  ];
}
