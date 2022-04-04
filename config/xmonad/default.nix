{ pkgs, config, nixosConfig, ... }:

{
  xsession = {
    enable = true;

    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      config = ./xmonad.hs;
      # TODO: turn this into a tree structure and have nix generate the haskell
      libFiles = {
        "MyAppTree.hs" = pkgs.writeText "MyAppTree.hs" ''
          module MyAppTree where

          import Data.Tree
          import XMonad.Core
          import XMonad.Actions.TreeSelect
          import XMonad.Config.Prime (spawn)

          myApps :: Forest (TSNode (X ()))
          myApps = 
            [ Node (TSNode "Chat" "Chat" (return ()))
                [ Node (TSNode "Discord" "Discord" (spawn "${pkgs.discord}/bin/Discord")) []
                , Node (TSNode "Zulip" "Z" (spawn "${pkgs.zulip}/bin/zulip")) []
                ]
            , Node (TSNode "Media creation" "Z" (return ()))
                [ Node (TSNode "Krita" "Z" (spawn "${pkgs.krita}/bin/krita")) []
                , Node (TSNode "Gimp" "Z" (spawn "${pkgs.gimp}/bin/gimp")) []
                , Node (TSNode "Inkscape" "Z" (spawn "${pkgs.inkscape}/bin/inkscape")) []
                , Node (TSNode "yEd" "Z" (spawn "${pkgs.yed}/bin/yed")) []
                , Node (TSNode "OBS" "Z" (spawn "${pkgs.obs-studio}/bin/obs")) []
                ]
            , Node (TSNode "Development" "Z" (return ()))
                [ Node (TSNode "VS code" "Z" (spawn "${pkgs.vscode}/bin/code")) []
                ]
            , Node (TSNode "Meetings" "video chat" (return ()))
                [ Node (TSNode "Zoom" "zoom" (spawn "${pkgs.zoom-us}/bin/zoom")) []
                , Node (TSNode "Jitsi" "jitsi" (spawn "${pkgs.jitsi-meet-electron}/bin/jitsi-meet-electron")) []
                , Node (TSNode "MS Teams" "teams" (spawn "${pkgs.teams}/bin/teams")) []
                , Node (TSNode "Skype" "skype" (spawn "${pkgs.skypeforlinux}/bin/skype")) []
                ]
            ]
        '';
      };
    };
  };
  home.file = {
    ###############################
    # XMonad utilities
    ###############################
    # TODO: make this configure based on machine capabilities
    ".xmonad/xmobar.hs".source = ./. + "/xmobar-${nixosConfig.networking.hostName}.hs";
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
