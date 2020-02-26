{pkgs, ...}:
{
  programs.alacritty = {
    enable = true;
    settings = {
      env = {
        "TERM" = "xterm-256color"; # fix weird server errors
        "WINIT_HIDPI_FACTOR" = "1"; # fix alacritty font scaling issue
      };
      window = {
        padding.x = 10;
        padding.y = 10;
        #decorations = "buttonless";
      };
      scrolling.history = 10000;
      url.launcher = "open";
      font = {
        size = 11;
        #use_thin_strokes = true;

        normal.family = "Anonymice Nerd Font";
        bold.family = "Anonymice Nerd Font";
        italic.family = "Anonymice Nerd Font";

      };
      # zenburn color scheme
      colors = {
        primary = {
          background = "0x383838";
          foreground = "0xdcdccc";
        };
        cursor = {
          text = "0x383838";
          cursor = "0xdcdccc";
        };
        normal = {
          black = "0x383838";
          red = "0xdca3a3";
          green = "0x5f7f5f";
          yellow = "0xe0cf9f";
          blue = "0x7cb8bb";
          magenta = "0xdc8cc3";
          cyan = "0x93e0e3";
          white = "0xdcdccc";
        };
        bright = {
          black = "0x6f6f6f";
          red = "0xdca3a3";
          green = "0x5f7f5f";
          yellow = "0xe0cf9f";
          blue = "0x7cb8bb";
          magenta = "0xdc8cc3";
          cyan = "0x93e0e3";
          white = "0xffffff";
        };
        indexed_colors = [
          {index=16; color="0xdfaf8f";}
          {index=17; color="0x000000";}
          {index=18; color="0x404040";}
          {index=19; color="0x606060";}
          {index=20; color="0x808080";}
          {index=21; color="0xc0c0c0";}
        ];
      };
    };
  };
}
