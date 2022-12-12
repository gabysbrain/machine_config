{ pkgs, config, nixosConfig, ... }:

{
  xsession = {
    enable = true;

    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      config = ./xmonad.hs;
      extraPackages = haskellPackages: [
        haskellPackages.dbus
      ];
    };
  };
  services.polybar = {
    enable = true;
    script = ''
      polybar main &
    '';
    settings = {
      "settings" = {
        screenchange-reload = true;
      };
      "bar/main" = {
        monitor = "DVI-D-0";
        width = "100%";
        height = "20";

        background = "#fefefe";
        foreground = "#333333";

        font-0 = "Anonymice Nerd Font:size=10";

        modules-left = "xmonad";
        modules-right = "date";
      };
      "module/xmonad" = {
        type = "custom/script";
        exec = "xmonad-log";
        tail = true;
      };
      "module/date" = {
        type = "internal/date";
        date = "%a %d %b";
        time = "%H:%M";
        label = "%date% | %time%";
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
  home.packages = with pkgs; [
    xmonad-log
  ];
}
