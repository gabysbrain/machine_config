{pkgs, ...}:
{
  home.packages = [
    pkgs.termite
  ];
  # FIXME: can't use fancy home-manager config because it's bugged
  xdg.configFile."termite/config".text = ''
      [options]
      browser = mimeo
      clickable_url = true
      font = Anonymice Nerd Font 11
      scrollback_lines = 10000

      # Copyright (c) 2016-present Arctic Ice Studio <development@arcticicestudio.com>
      # Copyright (c) 2016-present Sven Greb <code@svengreb.de>

      # Project:    Nord Termite
      # Repository: https://github.com/arcticicestudio/nord-termite
      # License:    MIT

      [colors]
      cursor = #d8dee9
      cursor_foreground = #2e3440

      foreground = #d8dee9
      foreground_bold = #d8dee9
      background = #2e3440

      highlight = #4c566a

      color0  = #3b4252
      color1  = #bf616a
      color2  = #a3be8c
      color3  = #ebcb8b
      color4  = #81a1c1
      color5  = #b48ead
      color6  = #88c0d0
      color7  = #e5e9f0
      color8  = #4c566a
      color9  = #bf616a
      color10 = #a3be8c
      color11 = #ebcb8b
      color12 = #81a1c1
      color13 = #b48ead
      color14 = #8fbcbb
      color15 = #eceff4
    '';
  programs.zsh.initExtra = ''
     # See https://github.com/thestinger/termite#id1
    if [[ $TERM == xterm-termite ]]; then
      . ${pkgs.gnome3.vte}/etc/profile.d/vte.sh
    fi
  '';
}