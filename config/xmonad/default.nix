{ pkgs, config, ... }:

let
  sysconfig = (import <nixpkgs/nixos> {}).config;
in
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
    # TODO: make this configure based on machine capabilities
    ".xmonad/xmobar.hs".source = ./. + "/xmobar-${sysconfig.networking.hostName}.hs";
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
