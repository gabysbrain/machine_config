{ config, pkgs, ... }:

# TODO: unify this with profiles/games
{
  # see https://nixos.wiki/wiki/Steam
  # needed for steam 32 bit
  hardware.graphics.enable32Bit = true;
  hardware.graphics.extraPackages32 = with pkgs.pkgsi686Linux; [ libva ];
  hardware.pulseaudio.support32Bit = true;

  programs.gamemode.enable = true;

  programs.steam.enable = true;
  programs.steam.gamescopeSession.enable = true;

  environment.systemPackages = with pkgs; [
    # launchers
    lutris
  ];
}
