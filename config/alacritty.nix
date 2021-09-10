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
        padding.x = 1;
        padding.y = 1;
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
      # Nord color scheme
      # see https://github.com/arcticicestudio/nord-alacritty/blob/develop/src/nord.yml
      colors = {
        # Default colors
        primary = {
          background = "0x2e3440";
          foreground = "0xd8dee9";
          dim_foreground = "0xa5abb6";
        };

        cursor = {
          text = "#2e3440";
          cursor = "#d8dee9";
        };

        vi_mode_cursor = {
          text = "#2e3440";
          cursor = "#d8dee9";
        };

        selection = {
          text = "CellForeground";
          background = "#4c566a";
        };

        search = {
          matches = {
            foreground = "CellBackground";
            background = "#88c0d0";
          };

          bar = {
            background = "#434c5e";
            foreground = "#d8dee9";
          };
        };

        # Normal colors
        normal = {
          black =   "0x3b4252";
          red =     "0xbf616a";
          green =   "0xa3be8c";
          yellow =  "0xebcb8b";
          blue =    "0x81a1c1";
          magenta = "0xb48ead";
          cyan =    "0x88c0d0";
          white =   "0xe5e9f0";
        };

        # dim colors
        dim = {
          black = "#373e4d";
          red = "#94545d";
          green = "#809575";
          yellow = "#b29e75";
          blue = "#68809a";
          magenta = "#8c738c";
          cyan = "#6d96a5";
          white = "#aeb3bb";
        };

        # Bright colors
        bright = {
          black =   "0x4c566a";
          red =     "0xbf616a";
          green =   "0xa3be8c";
          yellow =  "0xebcb8b";
          blue =    "0x81a1c1";
          magenta = "0xb48ead";
          cyan =    "0x8fbcbb";
          white =   "0xeceff4";
        };
      };
    };
  };
}
