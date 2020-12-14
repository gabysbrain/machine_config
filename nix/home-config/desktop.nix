{ pkgs, ... }:

{
  imports = [
    ./config/base.nix
    ./config/direnv.nix
    ./config/email.nix
    ./config/tasks.nix
    ./config/termite.nix
    ../pkgs/neovim/default.nix
  ];
  xsession = {
    enable = true;

    # run something like the first time: feh --bg-scale /home/tom/Dropbox/Wallpapers/future_past_japan.jpg
    initExtra = ''
      ~/.fehbg

      ${pkgs.gnome3.gnome-keyring}/bin/gnome-keyring-daemon --start -d --components=pkcs11,secrets,ssh
    '';
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      config = ../../xmonad/xmonad.hs;
    };
  };
  programs = {
    zathura = {
      enable = true;
      options = {
        selection-clipboard = "clipboard";
      };
      extraConfig = ''
        map <C-o> feedkeys ":exec okular $FILE<Return>"
      '';
    };
    browserpass = {
      enable = true;
      browsers = [ "chromium" "firefox" ];
    };
    obs-studio = {
      enable = true;
      plugins = with pkgs; [ 
        obs-wlrobs 
        obs-v4l2sink 

        obs-linuxbrowser
      ];
    };
  };
  home.file = {
    ###############################
    # Calendar/contacts sync
    ###############################
    ".vdirsyncer/config".source = ../../dotfiles/dot-vdirsyncer;
    ".config/khal/config".source = ../../dotfiles/dot-khal;
    ".config/khard/khard.conf".source = ../../dotfiles/dot-khard;

    ###############################
    # XMonad utilities
    ###############################
    ".xmonad/xmobar.hs".source = ../../xmonad/xmobar.hs;
    ".xmonad/net.sh" = {
      source = ../../xmonad/net.sh;
      executable = true;
    };
    ".xmonad/xmobar-syncthing-status.sh" = {
      source = ../../xmonad/xmobar-syncthing-status.sh;
      executable = true;
    };

    ###############################
    # Other stuff
    ###############################
    ".tvrc".source = ../../dotfiles/dot-tvrc;
    ".config/mimeo/associations.txt".source = ../../dotfiles/dot-associations;
    ".config/lf/lfrc".source = ../../dotfiles/dot-lfrc;
    ".newsboat/config".source = ../../dotfiles/dot-newsboat;
  };
  gtk = {
    enable = true;
    theme.name = "Arc-Darker";
    theme.package = pkgs.arc-theme;
    iconTheme.name = "Arc";
    iconTheme.package = pkgs.arc-icon-theme;
  };
  qt = {
    enable = true;
    platformTheme = "gtk";
  };
  services = {
    dunst = {
      enable = true;
      settings = {
        global = {
          font = "Anonymous Pro 10";
          frame_color = "#E5E9F0";
          separator_color = "#E5E9F0";

          format = "<b>%s</b>\\n%b";
          sort = "no";
          indicate_hidden = "yes";
          alignment = "left";
          show_age_threshold = -1;
          word_wrap = "yes";
          ignore_newline = "no";
          stack_duplicates = "yes";
          hide_duplicate_count = "yes";

          geometry = "300x50-15+49";
          shrink = "no";
          transparency = 5;
          idle_threshold = 0;
          monitor = 0;
          follow = "mouse";

          sticky_history = "yes";
          history_length = 15;
          show_indicators = "no";

          line_height = 3;
          separator_height = 2;
          padding = 6;
          horizontal_padding = 6;
          frame_width = 1;

          startup_notification = "true"; #false
          dmenu = "dmenu -p dunst:";
          browser = "firefox";
          icon_position = "off";
        };

        urgency_low = {
          background = "#5E81AC";
          foreground = "#ECEFF4";
          timeout = 2;
        };

        urgency_normal = {
          background = "#3B4252";
          foreground = "#ECEFF4";
          timeout = 4;
        };

        urgency_critical = {
          background = "#BF616A";
          foreground = "#ECEFF4";
          timeout = 0;
        };
      };
    };
    gpg-agent = {
      enable = true;
      defaultCacheTtl = 1800; # 30 minutes
      maxCacheTtl = 604800; # one week
      extraConfig = ''
        pinentry-program ${pkgs.pinentry_gtk2}/bin/pinentry
      '';
    };
  };
  systemd.user = {
    services = {
      vdirsyncer = {
        Unit = {
          Description="sync vcard/vcal servers";
        };
        Service = {
          ExecStart = "${pkgs.vdirsyncer}/bin/vdirsyncer sync";
        };
      };
    };
    timers = {
      vdirsyncer = {
        Unit = {
          Description="sync vcard/vcal servers";
        };
        Timer = {
          OnBootSec = "2m";
          OnUnitInactiveSec = "15m";
        };
        Install = {
          WantedBy = ["timers.target"];
        };
      };
    };
  };

  home.packages = [
    (pkgs.callPackage ../pkgs/preview.nix {})
    (pkgs.callPackage ../pkgs/syncmail {})
    pkgs.xkb-switch
    pkgs.isync
    pkgs.khard
    pkgs.khal
    pkgs.gmailieer

    # for reviewing papers
    (pkgs.callPackage ../pkgs/summ_paper {})

    # task management stuff
    pkgs.taskwarrior
    pkgs.timewarrior
    (pkgs.callPackage ../pkgs/tasks {})
  ];
}
