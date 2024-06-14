{ pkgs, ... }:

{
  home.packages = with pkgs; [
    discord
    mumble

    # steam comes from the above programs.steam option
    vassal # mmm board games
    frotz  # infocom games
    vassal # for wargames

    nethack
    wesnoth
  ];
}
