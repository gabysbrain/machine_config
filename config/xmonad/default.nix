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
        padding-left = 1;
        padding-right = 1;

        background = "#fefefe";
        foreground = "#333333";

        font-0 = "Anonymice Nerd Font:size=10";

        separator = " | ";
        modules-left = "xmonad";
        modules-right = "cpu wired-network audio date";
      };
      "module/xmonad" = {
        type = "custom/script";
        exec = "xmonad-log";
        tail = true;
      };
      "module/cpu" = {
        type = "internal/cpu";
        label = "CPU %percentage%";
      };
      "module/wired-network" = {
        type = "internal/network";
        interface = "enp4s0";
        format-connected = "<label-connected>";
        label-connected = "";
        label-disconnected = "%{F#dddddd}%{F-}";
        #label-connected = "直";
        #label-disconnected = "%{F#dddddd}睊%{F-}";
      };
      "module/audio" = {
        type = "internal/alsa";
        format-volume = "vol <label-volume>";
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
    #".xmonad/xmobar.hs".source = ./. + "/xmobar-${nixosConfig.networking.hostName}.hs";
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
