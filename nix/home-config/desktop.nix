{ pkgs, ... }:
{
  imports = [
    ./config/base.nix
    ./config/email.nix
    ./config/urxvt.nix
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

  home.packages = [
    (pkgs.callPackage ../pkgs/open.nix {})
    (pkgs.callPackage ../pkgs/preview.nix {})
    pkgs.xkb-switch
    pkgs.isync
    pkgs.haskellPackages.stylish-haskell
  ];
}
