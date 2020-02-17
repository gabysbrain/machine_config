{pkgs, ...}:
{
  programs.zsh  = {
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

      # restic backups
      restic-local = "restic -r sftp:backup@192.168.0.14:/backup/ -o 'sftp.command=ssh -i /home/tom/Sync/keys/diskstation.rsa backup@192.168.0.14 -s sftp'";
      restic-remote = "export $(cat /home/tom/Sync/keys/wasabi | xargs) && restic -r s3:https://s3.wasabisys.com/gabysbrain-restic";
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
}
