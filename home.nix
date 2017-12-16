{
  programs = {
    zsh = {
      enable = true;
      oh-my-zsh = {
        enable = true;
        theme = "kolo";
        plugins = [ "git" ];
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
  };
  home.file = {
    ###############################
    # Email/calendar/contacts sync
    ###############################
    ".vdirsyncer/config".source = ~/Projects/dotfiles/dotfiles/dot-vdirsyncer;
    ".offlineimaprc".source = ~/Projects/dotfiles/dotfiles/dot-offlineimaprc;
    ".muttrc".source = ~/Projects/dotfiles/dotfiles/dot-muttrc;
    ".mutt".source = ~/Projects/dotfiles/dotfiles/dot-mutt;
    ".msmtprc".source = ~/Projects/dotfiles/dotfiles/dot-msmtprc;
    ".khal/config".source = ~/Projects/dotfiles/dotfiles/dot-khal;
    ".config/khard/khard.conf".source = ~/Projects/dotfiles/dotfiles/dot-khard;
    ###############################
    # Xmonad, etc
    ###############################
    ".xmonad/xmonad.hs".source = ~/Projects/dotfiles/xmonad/xmonad.hs;
    ".xmonad/xmobar.hs".source = ~/Projects/dotfiles/xmonad/xmobar.hs;
  };
  programs.home-manager.enable = true;
  programs.home-manager.path = https://github.com/rycee/home-manager/archive/release-17.09.tar.gz;
}
