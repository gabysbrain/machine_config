{ pkgs, ... }:
{
  imports = [
    ./config/base.nix
    ./config/termite.nix
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
  };
  home.file = {
    ###############################
    # Calendar/contacts sync
    ###############################
    ".vdirsyncer/config".source = ../../dotfiles/dot-vdirsyncer;
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
    pkgs.gmailieer
  ];
}
