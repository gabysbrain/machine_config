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

      # Tender color scheme
      [colors]
      foreground = #eeeeee
      background = #282828
      cursor = #eeeeee
      
      color0  = #282828
      color1  = #f43753
      color2  = #c9d05c
      color3  = #ffc24b
      color4  = #b3deef
      color5  = #d3b987
      color6  = #73cef4
      color7  = #eeeeee
      color8  = #4c4c4c
      color9  = #f43753
      color10 = #c9d05c
      color11 = #ffc24b
      color12 = #b3deef
      color13 = #d3b987
      color14 = #73cef4
      color15 = #feffff
    '';
  programs.zsh.initExtra = ''
     # See https://github.com/thestinger/termite#id1
    if [[ $TERM == xterm-termite ]]; then
      . ${pkgs.gnome3.vte}/etc/profile.d/vte.sh
    fi
  '';
}
