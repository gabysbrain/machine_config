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
          rev = "1.2";
          sha256 = "0gryvkrzdlncahm3gsklhshhzx0n07rbsm83zyab0nj721lms6nn";
        };
      }
      { name = "clipboard";
        src = pkgs.fetchFromGitHub {
          owner = "zpm-zsh";
          repo = "clipboard";
          rev = "6d87244071f2751d2ebbcb386383f3a875d63e87";
          sha256 = "0b7nna4jz1ib217czizqrmdz0yw4apg3gl8kvxr0h10ygwmxh77x";
        };
      }
      { name = "zsh-image-extension";
        src = pkgs.fetchFromGitHub {
          owner = "gabysbrain";
          repo = "zsh-image-extension";
          rev = "master";
          sha256 = "1f55h59z6jz5ypxwhhxihm71537v6yn41hawwpmy8w1n97n54gk6";
        };
      }
    ];
    shellAliases = {
      open = "mimeo";

      # drag and drop things
      drag = "dragon --and-exit";
      drop = "dragon --target --and-exit";

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
      restic-local = "restic -r sftp:backup@diskstation.lan:/backup/ -o 'sftp.command=ssh -i /home/tom/keys/diskstation.rsa backup@diskstation.lan -s sftp'";
      restic-remote = "export $(cat /home/tom/keys/wasabi | xargs) && restic -r s3:https://s3.wasabisys.com/gabysbrain-restic";
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
      nix-install() { nix-env -iA $1; }

      # vim edit command line
      autoload edit-command-line
      zle -N edit-command-line
      bindkey -M vicmd '^V' edit-command-line

      # menu completion
      zstyle ':completion:*' menu select

      # fancy globbing
      setopt extendedglob

      # ctrl-g to open broot
      # br comes from home-manager broot zsh integration
      bindkey -s '^g' 'br\n'

      # from https://github.com/ohmyzsh/ohmyzsh/blob/master/plugins/history/history.plugin.zsh
      function hs {
        history 0 | grep -i $*
      }

      # open taskwarrior task in jira
      function jira_open {
        open `task _get "$1".jiraurl`
      }
    '';
  };
}
