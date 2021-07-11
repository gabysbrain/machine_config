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
      # Tender color scheme
      colors = {
        # Default colors
        primary = {
          background = "0x282828";
          foreground = "0xeeeeee";
        };

        # Normal colors
        normal = {
          black =   "0x282828";
          red =     "0xf43753";
          green =   "0xc9d05c";
          yellow =  "0xffc24b";
          blue =    "0xb3deef";
          magenta = "0xd3b987";
          cyan =    "0x73cef4";
          white =   "0xeeeeee";
        };

        # Bright colors
        bright = {
          black =   "0x4c4c4c";
          red =     "0xf43753";
          green =   "0xc9d05c";
          yellow =  "0xffc24b";
          blue =    "0xb3deef";
          magenta = "0xd3b987";
          cyan =    "0x73cef4";
          white =   "0xfeffff";
        };
      };
    };
  };
}
