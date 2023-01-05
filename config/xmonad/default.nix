{ pkgs, config, nixosConfig, ... }:

let
  syncthing-status = pkgs.runCommand "syncthing-status" {
    buildInputs = with pkgs; [
      (callPackage ../../pkgs/syncthing-quick-status.nix {})
    ];
  } ''
    mkdir -p $out/bin
    cp ${./syncthing-status.sh} $out/bin/syncthing-status
    sed -i "2 i export PATH=$PATH" $out/bin/syncthing-status
  '';
in
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
        #monitor = "DVI-D-0";
        monitor = "eDP1";
        width = "100%";
        height = "20";
        padding-left = 1;
        padding-right = 1;

        background = "#fefefe";
        foreground = "#333333";

        font-0 = "Anonymice Nerd Font:size=10";

        separator = " | ";
        modules-left = "xmonad";
        modules-right = "cpu syncthing wireless-network audio battery date";
      };
      "module/xmonad" = {
        type = "custom/script";
        exec = "${pkgs.xmonad-log}/bin/xmonad-log";
        tail = true;
      };
      "module/syncthing" = {
        type = "custom/script";
        exec = "${syncthing-status}/bin/syncthing-status";
        format = "st <label>";
      };
      "module/cpu" = {
        type = "internal/cpu";
        format = "cpu <label>";
        label = "%percentage%";
      };
      "module/wired-network" = {
        type = "internal/network";
        interface = "enp4s0";
        format-connected = "<label-connected>";
        label-connected = "";
        label-disconnected = "%{F#dddddd}%{F-}";
      };
      "module/wireless-network" = {
        type = "internal/network";
        interface = "wlp2s0";
        format-connected = "<label-connected>";
        label-connected = "直";
        label-disconnected = "%{F#dddddd}睊%{F-}";
      };
      "module/battery" = {
        type = "internal/battery";
        battery = "BAT1";
        format-charging = "bat <label-charging>";
        format-discharging = "bat <label-discharging>";
        label-charging = "%percentage%+";
        label-discharging = "%percentage% %time%";
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
    #".xmonad/net.sh" = {
      #source = ./net.sh;
      #executable = true;
    #};
  };
  home.packages = with pkgs; [
    xmonad-log
  ];
}
