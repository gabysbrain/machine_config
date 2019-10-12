{ pkgs, ... }:
{
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
  };
  home.sessionVariables = {
    EDITOR = "vim";
  };
  home.file = {
    ###############################
    # Ranger
    ###############################
    ".config/ranger/rifle.conf".source = ../../dotfiles/dot-rifle.conf;
    ".config/ranger/rc.conf".source = ../../dotfiles/dot-ranger/rc.conf;
    ".config/ranger/scope.sh".source = ../../dotfiles/dot-ranger/scope.sh;
    ".config/ranger/commands.py".source = ../../dotfiles/dot-ranger/commands.py;
    ".config/ranger/colorschemes/zenburn.py".source = ../../dotfiles/dot-ranger/zenburn.py;
  };
  services = {
    gpg-agent = {
      enable = true;
      defaultCacheTtl = 1800; # 30 minutes
      maxCacheTtl = 604800; # one week
    };
  };

  # TODO: integrate this into the programs.vim module
  home.packages = [
    (import ../pkgs/vim.nix)
  ];

  programs.home-manager.enable = true;
}
