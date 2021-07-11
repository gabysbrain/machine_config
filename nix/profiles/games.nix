{ pkgs, ... }:

{
  home.packages = with pkgs; [
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
