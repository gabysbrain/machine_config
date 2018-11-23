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
          flatten = ".";
          patterns = [ 
            "*" "INBOX"
            "![Gmail]/All Mail" 
            #"![Gmail]/Sent" 
            "![Gmail]/Trash" 
            "![Gmail]/Drafts" "![Gmail]/Spam" 
            #"![Gmail]/*"
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
          flatten = ".";
          patterns = [ 
            "*" "INBOX"
            "!Archive" "!Sent" "!Trash" "!Junk" "!Drafts"
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
      Master :personal-remote:"[Gmail]/Sent"
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
    ".tvrc".source = ../dotfiles/dot-tvrc;
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
  programs.home-manager.path = https://github.com/rycee/home-manager/archive/release-18.09.tar.gz;
}
