{ pkgs, homeage, ... }:
{
  imports = [
    ../config/broot/default.nix
    ../config/fzf.nix
    ../config/zsh.nix
    ../config/tmux.nix
    ../config/lf/default.nix

    ../config/neovim/default.nix

    #homeage.homeManagerModule.homeage
  ];
  homeage = {
    identityPaths = [ "~/.ssh/id_ed25519" ];

    # FIXME: systemd activation is buggy. Secrets aren't always created
    #installationType = "activation";
    installationType = "systemd";
  };
  programs = {
    git = {
      enable = true;
      lfs.enable = true;
      userName = "Thomas Torsney-Weir";
      userEmail = "torsneyt@gmail.com";
      extraConfig = {
        credential = {
          helper = "store";
        };
        pull.rebase = true;
        merge = {
          tool = "vimdiff";
          conflictstyle = "diff3";
        };
        mergetool.vimdiff.cmd = ''nvim -d -M "$LOCAL" "$MERGED" "$REMOTE" -c "wincmd w" -c "set modifiable" -c "set write"'';
        init = {
          defaultBranch = "dev";
        };
      };
    };
    bat = {
      enable = true;
      config = {
        pager = "less -FR";
        # TODO need to run `bat cache --build` after custom theme
        theme = "Nord";
      };
    };
    ssh = {
      enable = true;
      matchBlocks = {
        "gitlab.com" = {
          hostname = "gitlab.com";
          identityFile = "~/keys/id_gitlab";
        };
        "github.com" = {
          hostname = "github.com";
          identityFile = "~/keys/id_github";
        };
        "smaug.gt-d.com" = {
          hostname = "smaug.gt-d.com";
          identityFile = "~/keys/id_raicoon_gitlab";
        };
      };
    };
  };
  home.sessionVariables = {
    EDITOR = "vim";
    OPENER = "mimeo";
    BROWSER = "firefox";
  };
  home.packages = with pkgs; [
    gopass
    gopass-jsonapi
    gnupg

    silver-searcher

    nix-template
  ];

  #nixpkgs.config.allowUnfree = true;
  programs.home-manager.enable = true;
}
