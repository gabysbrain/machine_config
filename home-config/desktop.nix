{ pkgs, ... }:

{
  imports = [
    ../config/direnv.nix
    ../config/kitty.nix
    ../config/xmonad/default.nix
    ../config/mimeo/default.nix

    ../profiles/pim/default.nix
  ];
  nixpkgs.overlays = [
    # TODO: move the slock overlay to the user level
    #(import ../overlays/slock.nix)
  ];
  home.file.".xinitrc".text = "source ~/.xsession";
  xsession = {
    enable = true;

    # run something like the first time: feh --bg-scale /home/tom/Dropbox/Wallpapers/future_past_japan.jpg
    initExtra = ''
      ~/.fehbg

      ${pkgs.tmux}/bin/tmux kill-session -t Tasks
      ${pkgs.tmux}/bin/tmux start-server
      ${pkgs.gnome-keyring}/bin/gnome-keyring-daemon --start -d --components=pkcs11,secrets,ssh
    '';
  };
  programs = {
    ncmpcpp = {
      enable = true;
      settings = {
        mpd_host = "ttw.music.joukamachi.net";
      };
    };
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
        obs-studio-plugins.wlrobs
      ];
    };
  };

  gtk = {
    enable = true;
    theme.package = pkgs.juno-theme;
    theme.name = "Juno";
    iconTheme.package = pkgs.flat-remix-icon-theme;
    iconTheme.name = "Flat-Remix-Green-Dark";
  };
  qt = {
    enable = true;
    platformTheme.name = "gtk";
  };

  services = {
    picom = {
      enable = true;

      shadow = true;
      settings = {
        blur = { 
          method = "dual_kawase";
          strength = 8;
        };
        no-dnd-blur = true;
        unredir-if-possible = false;
      };

      # see https://nixos.wiki/wiki/Nvidia#Fix_app_flickering_with_Picom
      backend = "glx";
      vSync = true;
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
        pinentry-program ${pkgs.pinentry-gtk2}/bin/pinentry
      '';
    };
  };

  home.packages = with pkgs; [
    #haskellPackages.xmobar
    dmenu

    (callPackage ../pkgs/preview.nix {})
    xkb-switch

    # for reviewing papers
    (callPackage ../pkgs/summ_paper {})

    # needed for termite to access things
    termite.terminfo

    blueman
    wpa_supplicant_gui
    cifs-utils
    samba
    iwgtk
    pavucontrol
    helvum # pipewire patchbay
    btop # monitoring

    libreoffice

    inkscape
    darktable
    gimp
    krita
    scribus
    dia
    imagemagick
    ffmpeg
    bftools

    yt-dlp

    exiftool
    poppler_utils
    #glib-networking # feedreader needs this for ssl

    libnotify

    # ledger/finance stuff
    hledger
    hledger-interest

    peco # for history search

    xclip
    xdotool
    xdragon
    feh
    sxiv
    viu
    (callPackage ../pkgs/screenshot {})

    (callPackage ../pkgs/gopass-dmenu.nix {})
    (callPackage ../pkgs/syncthing-quick-status.nix {})

    #(callPackage ../pkgs/zk.nix {}) # notes

    firefox

    xournal # editing pdfs
    okular
    zathura
    breeze-icons # needed for okular

    silver-searcher
    unzip
    unrar
    p7zip
  ];
}
