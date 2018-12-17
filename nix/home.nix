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
  accounts.email = {
    maildirBasePath = ".mail";
    accounts = {
      personal = {
        primary = true;
        realName = "Tom Torsney-Weir";
        userName = "torsneyt@gmail.com";
        address = "torsneyt@gmail.com";
        flavor = "gmail.com";
        msmtp.enable = true;
        notmuch.enable = true;
        imap = {
          #host = "";
          tls.enable = true;
        };
        smtp = {
          #host = "";
          tls.enable = true;
        };
        mbsync = {
          enable = true;
          create = "both";
          expunge = "both";
          remove = "both";
          flatten = ".";
          patterns = [ 
            "*" "INBOX"
            "![Gmail]/*" "!sent" "!archive" "!spam" "!trash"
          ];
        };
        passwordCommand = "gpg --quiet --for-your-eyes-only --decrypt ~/.password-store/gmail/mbsync.gpg | head -1";
        folders = {
          inbox = "INBOX";
          #drafts = "[Gmail]/Drafts";
          #sent = "[Gmail]/Sent Mail";
          #trash = "[Gmail]/Trash";
          #archive = "[Gmail]/All Mail";
        };
      };
      work = {
        realName = "Tom Torsney-Weir";
        userName = "torsnet6";
        address = "thomas.torsney-weir@univie.ac.at";
        flavor = "plain";
        #offlineimap.enable = true;
        msmtp.enable = true;
        notmuch.enable = true;
        mbsync = {
          enable = true;
          create = "both";
          expunge = "both";
          remove = "both";
          flatten = ".";
          patterns = [ 
            "*" "INBOX"
            "!Archive" "!Sent" "!Trash" "!Junk" "!Drafts"
            "!archive" "!sent" "!trash" "!spam" "!drafts"
          ];
        };
        passwordCommand = "gpg --quiet --for-your-eyes-only --decrypt ~/.password-store/univie.ac.at.gpg | head -1";
        imap = {
          host = "imap.univie.ac.at";
          tls.enable = true;
        };
        smtp = {
          host = "mail.univie.ac.at";
          tls.enable = true;
        };
        folders = {
          inbox = "INBOX";
          #drafts = "INBOX.Drafts";
          #sent = "INBOX.Sent";
          #trash = "INBOX.Trash";
        };
      };
    };
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
    msmtp.enable = true; # sendmail support
    notmuch.enable = true; # index email
    mbsync = {
      enable = true; # imap mail sync support
      extraConfig = ''

      Channel personal-archive
      Master :personal-remote:"[Gmail]/All Mail"
      Slave :personal-local:"archive"
      Create Both
      Expunge Both
      SyncState *

      Channel personal-drafts
      Master :personal-remote:"[Gmail]/Drafts"
      Slave :personal-local:"drafts"
      Create Both
      Expunge Both
      SyncState *

      Channel personal-sent
      Master :personal-remote:"[Gmail]/Sent Mail"
      Slave :personal-local:"sent"
      Create Both
      Expunge Both
      SyncState *

      Channel personal-trash
      Master :personal-remote:"[Gmail]/Trash"
      Slave :personal-local:"trash"
      Create Both
      Expunge Both
      SyncState *

      Channel personal-spam
      Master :personal-remote:"[Gmail]/Spam"
      Slave :personal-local:"spam"
      Create Both
      Expunge Both
      SyncState *

      Channel work-archive
      Master :work-remote:"Archive"
      Slave :work-local:"archive"
      Create Both
      Expunge Both
      SyncState *

      Channel work-drafts
      Master :work-remote:"Drafts"
      Slave :work-local:"drafts"
      Create Both
      Expunge Both
      SyncState *

      Channel work-sent
      Master :work-remote:"Sent"
      Slave :work-local:"sent"
      Create Both
      Expunge Both
      SyncState *

      Channel work-trash
      Master :work-remote:"Trash"
      Slave :work-local:"trash"
      Create Both
      Expunge Both
      SyncState *

      Channel work-spam
      Master :work-remote:"Junk"
      Slave :work-local:"spam"
      Create Both
      Expunge Both
      SyncState *

      Group personal
      Channel personal
      Channel personal-archive
      Channel personal-drafts
      Channel personal-sent
      Channel personal-trash
      Channel personal-spam

      Group work
      Channel work
      Channel work-archive
      Channel work-drafts
      Channel work-sent
      Channel work-trash
      Channel work-spam
      '';
      groups = {
        inboxes = {
          work     = ["INBOX"];
          personal = ["INBOX"];
        };
      };
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
    ".vdirsyncer/config".source = ../dotfiles/dot-vdirsyncer;
    ".muttrc".source = ../dotfiles/dot-muttrc;
    ".mutt".source = ../dotfiles/dot-mutt;
    ".config/khal/config".source = ../dotfiles/dot-khal;
    ".config/khard/khard.conf".source = ../dotfiles/dot-khard;
    ".urlview".source = ../dotfiles/dot-urlview;

    ###############################
    # Xmonad, etc
    ###############################
    ".xmonad/xmonad.hs".source = ../xmonad/xmonad.hs;
    ".xmonad/xmobar.hs".source = ../xmonad/xmobar.hs;
    ".xmonad/wireless.sh" = {
      source = ../xmonad/wireless.sh;
      executable = true;
    };

    ###############################
    # Other stuff
    ###############################
    ".config/ranger/rifle.conf".source = ../dotfiles/dot-rifle.conf;
    ".config/ranger/rc.conf".source = ../dotfiles/dot-ranger/rc.conf;
    ".config/ranger/commands.py".source = ../dotfiles/dot-ranger/commands.py;
    ".tvrc".source = ../dotfiles/dot-tvrc;
    ".newsboat/config".source = ../dotfiles/dot-newsboat;
  };
  gtk = {
    enable = true;
    theme.name = "Arc";
    theme.package = pkgs.arc-theme;
    iconTheme.name = "Arc";
    iconTheme.package = pkgs.arc-icon-theme;
        #nixos.arc-icon-theme
  };
  services = {
    mbsync = {
      enable = true;
      frequency = "15m";
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
      "xft:Anonymous Pro:size=11"
      #"xft:Terminus:size=11"
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

    transparent = true;
    shading = 20;

    extraConfig = {
      #"letterSpace" = -1;
      "perl-ext-common" = "default,matcher,tabbedex";
      #"xftAntialias" = true;
      "depth" = 32;
      "foreground" = "#dcdccc";
      "background" = "#3f3f3f";
      "cursorColor" = "#aaaaaa";
      "colorUL" = "#366060";
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
      "url-launcher" = "/usr/bin/xdg-open";
      "matcher.button" = "1";
    };
  };

  home.stateVersion = "18.09";
  programs.home-manager.enable = true;
  programs.home-manager.path = https://github.com/rycee/home-manager/archive/release-18.09.tar.gz;
}
