{pkgs, ...}:
{
  # urxvt config
  programs.urxvt = {
    enable = true;
    package = pkgs.rxvt_unicode-with-plugins;
    fonts = [
      "xft:Anonymice Nerd Font:size=11"
    ];
    scroll = {
      bar.enable = true;
      scrollOnOutput = false;
      scrollOnKeystroke = true;
    };
    keybindings = {
    };

    extraConfig = {
      "urxvt.termName" = "xterm-256color";
      "perl-ext-common" = "default,matcher";
      "xftAntialias" = true;
      "depth" = 32;
      "foreground" = "#dcdccc";
      "background" = "#3f3f3f";
      "cursorColor" = "#aaaaaa";
      "colorUL" = "#669090";
      "underlineColor" = "#dfaf8f";
      "color0" = "#3f3f3f";
      "color1" = "#cc9393";
      "color2" = "#7f9f7f";
      "color3" = "#d0bf8f";
      "color4" = "#6ca0a3";
      "color5" = "#dc8cc3";
      "color6" = "#93e0e3";
      "color7" = "#dcdccc";
      "color8" = "#000000";
      "color9" = "#dca3a3";
      "color10" = "#bfebbf";
      "color11" = "#f0dfaf";
      "color12" = "#8cd0d3";
      "color13" = "#dc8cc3";
      "color14" = "#93e0e3";
      "color15" = "#ffffff";
      "url-launcher" = "mimeo";
      "matcher.button" = "1";
    };
  };
}
