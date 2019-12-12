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
        # imap through manual offlineimap
        #imap = {
          ##host = "imap.gmail.com";
          #tls.enable = true;
        #};
        smtp = {
          #host = "smtp.gmail.com";
          tls.enable = true;
        };
        passwordCommand = "gpg --quiet --for-your-eyes-only --decrypt ~/.password-store/gmail/mbsync.gpg | head -1";
        folders = {
          inbox = "INBOX";
          drafts = "[Gmail]/Drafts";
          sent = "[Gmail]/Sent Mail";
          trash = "[Gmail]/Trash";
          #archive = "[Gmail]/All Mail";
        };
        msmtp.enable = true;
        notmuch.enable = true;
        #offlineimap = {
          #enable = true;
        #};
      };
      work = {
        realName = "Thomas Torsney-Weir";
        userName = "t.d.torsney-weir@swansea.ac.uk";
        address = "t.d.torsney-weir@swansea.ac.uk";
        flavor = "plain";
        passwordCommand = "gpg --quiet --for-your-eyes-only --decrypt ~/.password-store/swansea.ac.uk.gpg | head -1";
        # imap through gmail/offlineimap
        #imap = {
          #host = "outlook.office365.com";
          #tls.enable = true;
        #};
        smtp = {
          host = "outlook.office365.com";
          port = 587;
          tls.enable = true;
          tls.useStartTls = true;
        };
        folders = {
          inbox = "INBOX";
          drafts = "[Gmail]/Drafts";
          sent = "[Gmail]/Sent Mail";
          trash = "[Gmail]/Trash";
          #archive = "[Gmail]/All Mail";
        };
        msmtp.enable = true;
        notmuch.enable = true;
        #offlineimap = {
          #enable = true;
        #};
      };
      univie = {
        realName = "Thomas Torsney-Weir";
        userName = "torsnet6";
        address = "thomas.torsney-weir@univie.ac.at";
        flavor = "plain";
        passwordCommand = "gpg --quiet --for-your-eyes-only --decrypt ~/.password-store/univie.ac.at.gpg | head -1";
        # imap through manual offlineimap
        #imap = {
          #host = "imap.univie.ac.at";
          #tls.enable = true;
        #};
        smtp = {
          host = "mail.univie.ac.at";
          tls.enable = true;
        };
        folders = {
          inbox = "INBOX";
          drafts = "INBOX.Drafts";
          sent = "INBOX.Sent";
          trash = "INBOX.Trash";
        };
        msmtp.enable = true;
        notmuch.enable = true;
        #offlineimap = {
          #enable = true;
        #};
      };
    };
  };
  programs = {
    zsh = {
      enable = true;
      #defaultKeymap = "vicmd";
      enableCompletion = true;
      plugins = [
        { name = "pure";
          src = pkgs.fetchFromGitHub {
            owner = "sindresorhus";
            repo = "pure";
            rev = "v1.11.0";
            sha256 = "0nzvb5iqyn3fv9z5xba850mxphxmnsiq3wxm1rclzffislm8ml1j";
          };
        }
        { name = "zsh-syntax-highlighting";
          src = pkgs.fetchFromGitHub {
            owner = "zsh-users";
            repo = "zsh-syntax-highlighting";
            rev = "0.6.0";
            sha256 = "0zmq66dzasmr5pwribyh4kbkk23jxbpdw4rjxx0i7dx8jjp2lzl4";
          };
        }
        { name = "zsh-peco-history";
          src = pkgs.fetchFromGitHub {
            owner = "jimeh";
            repo = "zsh-peco-history";
            rev = "0.9.1";
            sha256 = "1kadc2ylqxs9yrscbx4fxhcalj5k9bgakm2rpk6zk205kl36a2gg";
          };
        }
      ];
      shellAliases = {
        gvim = "vim -g";

        # Git stuff
        ga = "git add";
        gb = "git branch";
        gc = "git commit -v";
        gco = "git checkout";
        gd = "git diff";
        gl = "git log --oneline --stat";
        glq = "git whatchanged -p --abbrev-commit --pretty=medium";
        gp = "git push";
        gst = "git status";
        gu = "git pull --rebase";

        # list dir stack
        d = "dirs -v | head -10";

        # history aliases
        h = "history 0";
      };
      history = {
        extended = true;
        save = 1000;
        share = true;
        ignoreDups = true;
        expireDuplicatesFirst = true;
      };
      initExtra = ''
        # setup up autopushd
        setopt autopushd pushdignoredups

        # commands
        nix-search() {echo "Searching for '$1'..." ; nix-env -qaP --description \* | grep -i $1; }
        nix-install() { nix-env -iA $1; }

        # vim edit command line
        autoload edit-command-line
        zle -N edit-command-line
        bindkey -M vicmd '^V' edit-command-line

        # menu completion
        zstyle ':completion:*' menu select

        # fancy globbing
        setopt extendedglob

        bindkey -s '^o' 'lfcd\n'

        # from https://github.com/ohmyzsh/ohmyzsh/blob/master/plugins/history/history.plugin.zsh
        function hs {
          history 0 | grep -i $*
        }

        lfcd () {
            tmp="$(mktemp)"
            lf -last-dir-path="$tmp" "$@"
            if [ -f "$tmp" ]; then
                dir="$(cat "$tmp")"
                rm -f "$tmp"
                if [ -d "$dir" ]; then
                    if [ "$dir" != "$(pwd)" ]; then
                        cd "$dir"
                    fi
                fi
            fi
        }
      '';
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
    bat = {
      enable = true;
      config = {
        pager = "less -FR";
        theme = "zenburn";
      };
    };
    offlineimap.enable = true; # email syncing
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
    ".offlineimaprc".source = ../../dotfiles/dot-offlineimap;
    ".offlineimap.py".source = ../../dotfiles/dot-offlineimap.py;

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

  # TODO: integrate this into the programs.vim module
  home.packages = [
    (pkgs.callPackage ../pkgs/vim.nix {})
    (pkgs.callPackage ../pkgs/open.nix {})
    (pkgs.callPackage ../pkgs/preview.nix {})
    pkgs.xkb-switch
    pkgs.isync
    pkgs.haskellPackages.stylish-haskell
  ];

  #home.stateVersion = "18.09";
  programs.home-manager.enable = true;
  #programs.home-manager.path = https://github.com/rycee/home-manager/archive/release-18.09.tar.gz;
}
