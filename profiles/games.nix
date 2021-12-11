{ pkgs, ... }:

{
  home.packages = with pkgs; [
    discord
    mumble

    # steam comes from the above programs.steam option
    vassal # mmm board games
    wyvern # GOG games

    minecraft
    nethack
    wesnoth
    zeroad # 0 a.d.
  ];
}
