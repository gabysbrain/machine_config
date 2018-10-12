{ pkgs, ... }:
{
  xsession = {
    enable = true;

    # run something like the first time: feh --bg-scale /home/tom/Dropbox/Wallpapers/future_past_japan.jpg
    initExtra = ''
      ~/.fehbg
    '';
    windowManager.command = 
      let
        xmonad = pkgs.xmonad-with-packages.override {
          packages = self: [ self.xmonad-contrib ];
        };
      in
        "${xmonad}/bin/xmonad";
  };
  xresources.properties = {
    "URxvt.perl-ext-common" = "default,matcher,tabbedex";
    "!URxvt.font" = "xft:Terminus:pixelsize=12";
    "URxvt*transparent" = true;
    "URxvt*shading" = 20;
    "URxvt*termName" = "rxvt-256color";
    "URxvt*xftAntialias" = true;
    "URxvt*background" = "#3f3f3f";
    "!URxvt*depth" = 32;
    "!URxvt*background" = "rgba:3f00/3f00/3f00/c800";
    "URxvt*foreground" = "#dcdccc";
    "URxvt*cursorColor" = "#aaaaaa";
    "URxvt*colorUL" = "#366060";
    "URxvt*underlineColor" = "#dfaf8f";
    "URxvt*color0" = "#3f3f3f";
    "URxvt*color1" = "#cc9393";
    "URxvt*color2" = "#7f9f7f";
    "URxvt*color3" = "#d0bf8f";
    "URxvt*color4" = "#6ca0a3";
    "URxvt*color5" = "#dc8cc3";
    "URxvt*color6" = "#93e0e3";
    "URxvt*color7" = "#dcdccc";
    "URxvt*color8" = "#000000";
    "URxvt*color9" = "#dca3a3";
    "URxvt*color10" = "#bfebbf";
    "URxvt*color11" = "#f0dfaf";
    "URxvt*color12" = "#8cd0d3";
    "URxvt*color13" = "#dc8cc3";
    "URxvt*color14" = "#93e0e3";
    "URxvt*color15" = "#ffffff";
    "URxvt*scrollTtyOutput" = false;
    "URxvt*scrollWithBuffer" = true;
    "URxvt*scrollTtyKeypress" = true;
    "URxvt.keysym.Control-t" = "perl:tabbedex:new_tab";
    "URxvt.keysym.Control-Tab" = "perl:tabbedex:next_tab";
    "URxvt.keysym.Control-Shift-Tab" = "perl:tabbedex:prev_tab";
    "URxvt.keysym.Control-Shift-Left" = "perl:tabbedex:move_tab_left";
    "URxvt.keysym.Control-Shift-Right" = "perl:tabbedex:move_tab_right";
    "URxvt.keysym.Control-Shift-R" = "perl:tabbedex:rename_tab";
    "URxvt.url-launcher" = "/usr/bin/xdg-open";
    "URxvt.matcher.button" = "1";
  };
  programs = {
    zsh = {
      enable = true;
      oh-my-zsh = {
        enable = true;
        theme = "kolo";
        plugins = [ "vi-mode" "history" "git" "stack" ];
      };
      shellAliases = {
        gvim = "vim -g";
      };
    };
    git = {
      enable = true;
      userName = "Thomas Torsney-Weir";
      userEmail = "torsneyt@gmail.com";
    };
    browserpass = {
      enable = true;
      browsers = [ "chromium" "firefox" ];
    };
  };
  home.sessionVariables = {
    EDITOR = "vim";
    BROWSER = "firefox";
  };
  home.file = {
    ###############################
    # Email/calendar/contacts sync
    ###############################
    ".vdirsyncer/config".source = ~/Projects/dotfiles/dotfiles/dot-vdirsyncer;
    ".offlineimaprc".source = ~/Projects/dotfiles/dotfiles/dot-offlineimaprc;
    ".muttrc".source = ~/Projects/dotfiles/dotfiles/dot-muttrc;
    ".mutt".source = ~/Projects/dotfiles/dotfiles/dot-mutt;
    ".msmtprc".source = ~/Projects/dotfiles/dotfiles/dot-msmtprc;
    ".notmuch-config".source = ~/Projects/dotfiles/dotfiles/dot-notmuch;
    ".config/khal/config".source = ~/Projects/dotfiles/dotfiles/dot-khal;
    ".config/khard/khard.conf".source = ~/Projects/dotfiles/dotfiles/dot-khard;
    ".urlview".source = ~/Projects/dotfiles/dotfiles/dot-urlview;

    ###############################
    # Xmonad, etc
    ###############################
    ".xmonad/xmonad.hs".source = ~/Projects/dotfiles/xmonad/xmonad.hs;
    ".xmonad/xmobar.hs".source = ~/Projects/dotfiles/xmonad/xmobar.hs;
    ".xmonad/wireless.sh" = {
      source = ~/Projects/dotfiles/xmonad/wireless.sh;
      executable = true;
    };

    ###############################
    # Other stuff
    ###############################
    ".tvrc".source = ~/Projects/dotfiles/dotfiles/dot-tvrc;
  };
  systemd.user = {
    services = {
      offlineimap = {
        Unit = {
          Description = "sync email servers";
        };
        Service = {
          ExecStart = "${pkgs.offlineimap}/bin/offlineimap";
        };
      };
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
      offlineimap = {
        Unit = {
          Description = "sync email servers";
        };
        Timer = {
          OnBootSec = "2m";
          OnUnitInactiveSec = "15m";
        };
        Install = {
          WantedBy = ["timers.target"];
        };
      };
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

  programs.home-manager.enable = true;
  programs.home-manager.path = https://github.com/rycee/home-manager/archive/release-18.03.tar.gz;
}
