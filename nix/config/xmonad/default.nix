{ pkgs, ... }:

{
  xsession = {
    enable = true;

    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      config = ./xmonad.hs;
    };
  };
  home.file = {
    ###############################
    # XMonad utilities
    ###############################
    ".xmonad/xmobar.hs".source = ./xmobar.hs;
    ".xmonad/net.sh" = {
      source = ./net.sh;
      executable = true;
    };
    ".xmonad/xmobar-syncthing-status.sh" = {
      source = ./xmobar-syncthing-status.sh;
      executable = true;
    };
  };
}
