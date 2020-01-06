{ pkgs, ... }:
{
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
        { name = "zsh-system-clipboard";
          src = pkgs.fetchFromGitHub {
            owner = "zsh-vi-more";
            repo = "evil-registers";
            rev = "master";
            sha256 = "0jsybyb3n9h1nxs3w1kh8mxmib8dx7hwch8i55qvr0xylcbcg791";
          };
        }
        { name = "clipboard";
          src = pkgs.fetchFromGitHub {
            owner = "zpm-zsh";
            repo = "clipboard";
            rev = "master";
            sha256 = "1vwihqcdzhrv463025p0g5wiccl19yr2c4zps325d3ylf3gxyzqy";
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

        # tmux stuff
        ta = "tmux attach -t";
        tad = "tmux attach -d -t";
        ts = "tmux new-session -s";
        tl = "tmux list-sessions";
        tkss = "tmux kill-session -t";
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
    tmux = {
      enable = true;
      shortcut = "a";
      terminal = "tmux-256color";
      extraConfig = ''
        # reload config
        unbind r
        bind r source-file ~/.tmux.conf

        # split panes using | and -
        bind | split-window -h
        bind - split-window -v
        unbind '"'
        unbind %

        # Fast pane switching
        bind -n M-Left select-pane -L
        bind -n M-Right select-pane -R
        bind -n M-Up select-pane -U
        bind -n M-Down select-pane -D

        #urxvt tab like window switching (-n: no prior escape seq)
        bind -n S-down new-window
        bind -n S-left prev
        bind -n S-right next
        bind -n C-left swap-window -t -1
        bind -n C-right swap-window -t +1

        # Turn on mouse mode
        set -g mouse on

        # status bar styling
        set-option -g status-position top

        # Zenburn colors
        setw -g clock-mode-colour colour117
        setw -g mode-style 'fg=colour117 bg=colour238 bold'
        set -g status-style 'fg=colour248 bg=colour235'
        setw -g window-status-current-style 'fg=colour223 bg=colour237 bold'
        set -g message-style 'fg=colour117 bg=colour235 bold'
        set -g status-left '#[fg=colour187,bold]'
      '';
    };
    git = {
      enable = true;
      userName = "Thomas Torsney-Weir";
      userEmail = "torsneyt@gmail.com";
    };
    bat = {
      enable = true;
      config = {
        pager = "less -FR";
        theme = "zenburn";
      };
    };
  };
  home.sessionVariables = {
    EDITOR = "vim";
    BROWSER = "firefox";
  };
  home.packages = [
    # TODO: integrate this into the programs.vim module
    (pkgs.callPackage ../../pkgs/vim.nix {})
    (pkgs.callPackage ../../pkgs/tat {})
  ];

  #home.stateVersion = "18.09";
  programs.home-manager.enable = true;
}
