{pkgs, lib, ...}:

let 
  tmux-projs = pkgs.callPackage ../pkgs/tmux-projs {};
in
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
      restic-remote = "export $(cat /home/tom/keys/wasabi | xargs) && restic -r s3:https://s3.eu-central-1.wasabisys.com/gabysbrain-restic";

      # I can never remember the command to fill pdfs
      fillpdf = "${pkgs.xournal}/bin/xournal";
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

      function ff {
        find . -name "$1" ''${@:2}
      }

      # from https://github.com/ohmyzsh/ohmyzsh/blob/master/plugins/history/history.plugin.zsh
      function hs {
        history 0 | grep -i $*
      }

      # function for easy port forwarding
      function fwdport {
        readonly conn=''${1:?"The host must be specified"}
        readonly myport=''${2:?"The port must be specified"}

        # if only one port is given then just use it for both
        if [ -z "$3" ]; then
          readonly dstport=$myport
        else
          readonly dstport=$3
        fi

        #ssh -N -L ''${srcport}:''${host}:''${dstport} ''${conn}
        ssh -N -L ''${myport}:localhost:''${dstport} ''${conn}
      }

      # ctrl-g to open directory search
      bindkey -s '^g' '${tmux-projs}/bin/tmux-projs\n'

      if [ -f ~/.config/zsh/scratch.zsh ]; then
        source ~/.config/zsh/scratch.zsh
      fi
    '';
  };
  programs.fzf = {
    enable = true; # needed for fzf history search
    enableZshIntegration = true;
  };
  home.activation = {
    zshScratchFiles = lib.hm.dag.entryAfter ["writeBoundary"] ''
      if [ ! -f $HOME/.config/zsh/scratch.zsh ]; then
        $DRY_RUN_CMD mkdir -p $HOME/.config/zsh/
        $DRY_RUN_CMD touch $HOME/.config/zsh/scratch.zsh
      fi
    '';
  };
}
