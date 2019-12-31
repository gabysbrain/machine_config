{ pkgs, ... }:
{
  imports = [
    ./config/base.nix
    ./config/email.nix
  ];
  xsession = {
    enable = true;

    # run something like the first time: feh --bg-scale /home/tom/Dropbox/Wallpapers/future_past_japan.jpg
    initExtra = ''
      ~/.fehbg
    '';
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      config = ../../xmonad/xmonad.hs;
    };
  };
  programs = {
    browserpass = {
      enable = true;
      browsers = [ "chromium" "firefox" ];
    };
    offlineimap.enable = true; # email syncing
    msmtp.enable = true; # sendmail support
    notmuch.enable = true; # index email
  };
  home.file = {
    ###############################
    # Email/calendar/contacts sync
    ###############################
    ".vdirsyncer/config".source = ../../dotfiles/dot-vdirsyncer;
    ".muttrc".source = ../../dotfiles/dot-muttrc;
    ".mutt".source = ../../dotfiles/dot-mutt;
    ".config/khard/khard.conf".source = ../../dotfiles/dot-khard;
    ".urlview".source = ../../dotfiles/dot-urlview;
    ".offlineimaprc".source = ../../dotfiles/dot-offlineimap;
    ".offlineimap.py".source = ../../dotfiles/dot-offlineimap.py;

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
    ".xmonad/kb-status.sh" = {
      source = ../../xmonad/kb-status.sh;
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
        #nixos.arc-icon-theme
  };
  qt = {
    enable = true;
    platformTheme = "gtk";
  };
  services = {
    gpg-agent = {
      enable = true;
      defaultCacheTtl = 1800; # 30 minutes
      maxCacheTtl = 604800; # one week
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

  # urxvt config
  programs.urxvt = {
    enable = true;
    package = pkgs.rxvt_unicode-with-plugins;
    fonts = [
      "xft:Anonymice Powerline:size=11"
    ];
    scroll = {
      bar.enable = true;
      scrollOnOutput = false;
      scrollOnKeystroke = true;
    };
    keybindings = {
      "Control-t" = "perl:tabbedex:new_tab";
      "Control-Tab" = "perl:tabbedex:next_tab";
      "Control-Shift-Tab" = "perl:tabbedex:prev_tab";
      "Control-Shift-Left" = "perl:tabbedex:move_tab_left";
      "Control-Shift-Right" = "perl:tabbedex:move_tab_right";
      "Control-Shift-R" = "perl:tabbedex:rename_tab";
    };

    extraConfig = {
      "urxvt.termName" = "xterm-256color";
      "perl-ext-common" = "default,matcher,tabbedex";
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

  home.packages = [
    (pkgs.callPackage ../pkgs/open.nix {})
    (pkgs.callPackage ../pkgs/preview.nix {})
    pkgs.xkb-switch
    pkgs.isync
    pkgs.haskellPackages.stylish-haskell
  ];
}
