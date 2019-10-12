{ pkgs, ... }:
{
  xsession = {
    enable = true;

    # run something like the first time: feh --bg-scale /home/tom/Dropbox/Wallpapers/future_past_japan.jpg
    initExtra = ''
      ~/.fehbg
    '';
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      config = ~/Projects/machine_config/xmonad/xmonad.hs;
    };
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
        #mbsync = {
          #enable = true;
          #create = "both";
          #expunge = "both";
          #remove = "both";
          #flatten = ".";
          #patterns = [ 
            #"*" "INBOX"
            #"![Gmail]/*" "!sent" "!archive" "!spam" "!trash"
          #];
          #extraConfig.account = {
            #PipelineDepth = 50;
            #Timeout = 60;
          #};
        #};
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
        realName = "Thomas Torsney-Weir";
        userName = "t.d.torsney-weir@swansea.ac.uk";
        address = "t.d.torsney-weir@swansea.ac.uk";
        flavor = "plain";
        msmtp.enable = true;
        notmuch.enable = true;
        #mbsync = {
          #enable = true;
          #create = "both";
          #expunge = "both";
          #remove = "both";
          #flatten = ".";
          #patterns = [ 
            #"*" 
            #"!Calendar*" "!Contacts"
            #"!Conversation History*" "!Journal" "!Notes" "!Tasks"
            #"!RSS Subscriptions*"
            #"!Outbox" "!Sync Issues*"
            #"!students"
            #"!Archive" "!Sent Items" "!Deleted Items" "!Junk Email" "!Drafts"
            #"!archive" "!sent" "!trash" "!spam" "!drafts"
          #];
        #};
        passwordCommand = "gpg --quiet --for-your-eyes-only --decrypt ~/.password-store/swansea.ac.uk.gpg | head -1";
        imap = {
          host = "outlook.office365.com";
          tls.enable = true;
        };
        smtp = {
          host = "outlook.office365.com";
          port = 587;
          tls.enable = true;
          tls.useStartTls = true;
        };
        folders = {
          inbox = "INBOX";
          #drafts = "INBOX.Drafts";
          #sent = "INBOX.Sent";
          #trash = "INBOX.Trash";
        };
      };
      univie = {
        realName = "Thomas Torsney-Weir";
        userName = "torsnet6";
        address = "thomas.torsney-weir@univie.ac.at";
        flavor = "plain";
        #offlineimap.enable = true;
        msmtp.enable = true;
        notmuch.enable = true;
        #mbsync = {
          #enable = true;
          #create = "both";
          #expunge = "both";
          #remove = "both";
          #flatten = ".";
          #patterns = [ 
            #"*" "INBOX"
            #"!Archive" "!Sent" "!Trash" "!Junk" "!Drafts"
            #"!archive" "!sent" "!trash" "!spam" "!drafts"
          #];
        #};
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
      initExtra = ''
        nix-search() {echo "Searching for '$1'..." ; nix-env -qaP --description \* | grep -i $1; }
        nix-install() { nix-env -iA $1; }
      '';
      history.ignoreDups = true;
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
  };
  home.sessionVariables = {
    EDITOR = "vim";
    BROWSER = "firefox";
  };
  home.file = {
    ###############################
    # Email/calendar/contacts sync
    ###############################
    ".vdirsyncer/config".source = ../../dotfiles/dot-vdirsyncer;
    ".muttrc".source = ../../dotfiles/dot-muttrc;
    ".mutt".source = ../../dotfiles/dot-mutt;
    ".config/khal/config".source = ../../dotfiles/dot-khal;
    ".config/khard/khard.conf".source = ../../dotfiles/dot-khard;
    ".urlview".source = ../../dotfiles/dot-urlview;
    ".mbsyncrc".source = ../../dotfiles/dot-mbsyncrc;

    ###############################
    # XMonad utilities
    ###############################
    ".xmonad/xmobar.hs".source = ../../xmonad/xmobar.hs;
    ".xmonad/wireless.sh" = {
      source = ../../xmonad/wireless.sh;
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
    # Ranger
    ###############################
    ".config/ranger/rifle.conf".source = ../../dotfiles/dot-rifle.conf;
    ".config/ranger/rc.conf".source = ../../dotfiles/dot-ranger/rc.conf;
    ".config/ranger/scope.sh".source = ../../dotfiles/dot-ranger/scope.sh;
    ".config/ranger/commands.py".source = ../../dotfiles/dot-ranger/commands.py;
    ".config/ranger/colorschemes/zenburn.py".source = ../../dotfiles/dot-ranger/zenburn.py;

    ###############################
    # Other stuff
    ###############################
    ".tvrc".source = ../dotfiles/dot-tvrc;
    ".newsboat/config".source = ../dotfiles/dot-newsboat;
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
    mbsync = {
      enable = true;
      frequency = "*:0/15";
    };
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

  # TODO: integrate this into the programs.vim module
  home.packages = [
    (import ../pkgs/vim.nix)
    pkgs.xkb-switch
    pkgs.isync
    pkgs.haskellPackages.stylish-haskell
  ];

  #home.stateVersion = "18.09";
  programs.home-manager.enable = true;
  #programs.home-manager.path = https://github.com/rycee/home-manager/archive/release-18.09.tar.gz;
}
