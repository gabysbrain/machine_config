{ pkgs, ... }:

{
  imports = [
    ../config/alacritty.nix
    ../config/direnv.nix
    ../config/pim.nix
    #../config/termite.nix
    ../config/xmonad/default.nix
    ../config/mimeo/default.nix
  ];
  xsession = {
    enable = true;

    # run something like the first time: feh --bg-scale /home/tom/Dropbox/Wallpapers/future_past_japan.jpg
    initExtra = ''
      ~/.fehbg

      ${pkgs.tmux}/bin/tmux kill-session -t Tasks
      ${pkgs.tmux}/bin/tmux start-server
      ${pkgs.gnome3.gnome-keyring}/bin/gnome-keyring-daemon --start -d --components=pkcs11,secrets,ssh
    '';
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
    obs-studio = {
      enable = true;
      plugins = with pkgs; [ 
        obs-wlrobs 
        obs-v4l2sink 

        #obs-linuxbrowser
      ];
    };
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
    picom = {
      enable = true;
      experimentalBackends = true;

      shadow = true;
      blur = true;

      extraOptions = ''
        blur-method = "dual_kawase";
        blur-strength = 8;
        no-dnd-blur = true;

        unredir-if-possible = false;
      '';

      # see https://nixos.wiki/wiki/Nvidia#Fix_app_flickering_with_Picom
      backend = "glx";
      vSync = true;

      package = pkgs.picom.overrideAttrs(o: {
        src = pkgs.fetchFromGitHub {
          repo = "picom";
          owner = "ibhagwan";
          rev = "60eb00ce1b52aee46d343481d0530d5013ab850b";
          sha256 = "1m17znhl42sa6ry31yiy05j5ql6razajzd6s3k2wz4c63rc2fd1w";
        };
      });
    };
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

  home.packages = with pkgs; [
    haskellPackages.xmobar
    dmenu

    (callPackage ../pkgs/preview.nix {})
    xkb-switch

    # for reviewing papers
    (callPackage ../pkgs/summ_paper {})

    # needed for termite to access things
    termite.terminfo

    blueman
    wpa_supplicant_gui
    connman-gtk
    connman_dmenu
    pavucontrol

    libreoffice-unwrapped
    spotify
    discord
    zotero
    meld

    inkscape
    darktable
    gimp
    imagemagick
    shotcut
    xfce.ristretto
    xfce.tumbler # needed for ristretto image previews

    youtube-dl

    exiftool
    poppler_utils
    #glib-networking # feedreader needs this for ssl

    libnotify

    # ledger/finance stuff
    hledger
    hledger-interest

    mimeo
    peco

    xclip
    xdotool
    feh
    sxiv
    (callPackage ../pkgs/screenshot {})

    (callPackage ../pkgs/gopass-dmenu.nix {})
    (callPackage ../pkgs/syncthing-quick-status.nix {})

    (haskellPackages.callPackage ../pkgs/zk {}) # notes
    obsidian

    firefox

    okular
    zathura
    breeze-icons # needed for okular

    silver-searcher
    unzip
    unrar
    p7zip
  ];
}
