{ pkgs, ... }:

let 
  ethInterface = "enp4s0";
  wifiInterface = "wlp2s0";
  syncthing-status = pkgs.runCommand "syncthing-status" {
    buildInputs = with pkgs; [
      (callPackage ../../pkgs/syncthing-quick-status.nix {})
    ];
  } ''
    mkdir -p $out/bin
    cp ${./syncthing-status.sh} $out/bin/syncthing-status
    sed -i "2 i export PATH=$PATH" $out/bin/syncthing-status
  '';
  # use nix to handle colors rather than a section in the ini file
  colors = {
    bg = "#fefefe";
    fg = "#333333";
    fg-alt = "#999999";
    lightgray = "#cccccc";

    # nord colors
    #bg = "#2E3440";
    #bg-alt = "#3B4252";
    #fg = "#ECEFF4";
    #fg-alt = "#E5E9F0";

    #blue = "#81A1C1";
    #cyan = "#88C0D0";
    #green = "#A3BE8C";
    #orange = "#D08770";
    #purple = "#B48EAD";
    red = "#BF616A";
    #yellow = "#EBCB8B";

    #black = "#000";
    #white = "#FFF";

    #trans = "#00000000";
    #semi-trans-black = "#aa000000";
  };
  wlan-config = "${pkgs.iwgtk}/bin/iwgtk";
  pipewire-vol = "${pkgs.callPackage ../../pkgs/pipewire-vol {}}/bin/pipewire-vol";
in
{
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
        #monitor = "eDP1";
        width = "100%";
        height = "20";
        padding-left = 1;
        padding-right = 1;

        background = colors.bg;
        foreground = colors.fg;

        font-0 = "Anonymous Pro:size=10";

        separator = " | ";
        modules-left = "xmonad";
        modules-right = "cpu syncthing wired-net wireless-net audio battery date";
      };
      "module/xmonad" = {
        type = "custom/script";
        exec = "${pkgs.xmonad-log}/bin/xmonad-log";
        tail = true;
      };
      "module/syncthing" = {
        type = "custom/script";
        exec = "${syncthing-status}/bin/syncthing-status";
        #format = "st <label>";
        format-prefix = "st";
        format-prefix-foreground = colors.fg-alt;
        format-prefix-padding = 1;
      };
      "module/cpu" = {
        type = "internal/cpu";
        #format = "<label>";
        format-prefix = "cpu";
        format-prefix-foreground = colors.fg-alt;
        format-prefix-padding = 1;
        label = "%percentage%";
      };
      "module/wired-net" = {
        type = "internal/network";
        interface = ethInterface;

        format-connected = "<label-connected>";
        format-connected-prefix = "eth";
        format-connected-prefix-foreground = colors.fg-alt;
        format-connected-prefix-padding = 1;
        format-disconnected = "<label-disconnected>";
        format-disconnected-prefix = "eth";
        format-disconnected-prefix-foreground = colors.fg-alt;
        format-disconnected-prefix-padding = 1;

        label-connected = "󰈁";
        label-disconnected = "%{F#dddddd}󰈂%{F-}";
      };
      "module/wireless-net" = {
        type = "internal/network";
        interface = wifiInterface;

        format-connected = "<label-connected>";
        format-connected-prefix = "wifi";
        format-connected-prefix-foreground = colors.fg-alt;
        format-connected-prefix-padding = 1;
        format-disconnected = "<label-disconnected>";
        format-disconnected-prefix = "wifi";
        format-disconnected-prefix-foreground = colors.fg-alt;
        format-disconnected-prefix-padding = 1;

        label-connected = "%{A1:${wlan-config}:}%{A}";
        label-disconnected = "%{F#dddddd}%{A1:${wlan-config}:}󰖪%{A}%{F-}";
      };
      "module/battery" = {
        type = "internal/battery";
        battery = "BAT1";

        full-at = 79;
        low-at = 5;

        time-format = "%H:%M";
        format-charging = "<label-charging>";
        format-discharging = "<label-discharging>";
        label-charging = "%percentage%+";
        label-discharging = "%percentage% %time%";
        label-full = "%percentage%";

        format-low-prefix = "bat";
        format-low-foreground = colors.red;
        format-low-prefix-padding = 1;

        format-full-prefix = "bat";
        format-full-prefix-foreground = colors.fg-alt;
        format-full-prefix-padding = 1;
        format-charging-prefix = "bat";
        format-charging-prefix-foreground = colors.fg-alt;
        format-charging-prefix-padding = 1;
        format-discharging-prefix = "bat";
        format-discharging-prefix-foreground = colors.fg-alt;
        format-discharging-prefix-padding = 1;
      };
      "module/audio" = {
        type = "custom/script";

        exec = "${pipewire-vol} -p '${colors.fg}' -m '${colors.lightgray}'";
        interval = 10;

        format = "<label>";
        format-prefix = "vol";
        format-prefix-foreground = colors.fg-alt;
        format-prefix-padding = 1;
      };
      "module/date" = {
        type = "internal/date";
        date = "%a %d %b";
        time = "%H:%M";
        label = "%date% | %time%";
      };
    };
  };
}
