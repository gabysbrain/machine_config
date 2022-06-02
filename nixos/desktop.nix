{ config, pkgs, ... }:

let system-icons = pkgs.callPackage ../pkgs/system-icons {};
in
{
  # Select internationalisation properties.
  # i18n = {
  #   consoleFont = "Lat2-Terminus16";
  #   consoleKeyMap = "us";
  #   defaultLocale = "en_US.UTF-8";
  # };

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  nixpkgs.config = {
    allowUnfree = true;
  };

  nixpkgs.overlays = [
    (import ../overlays/slock.nix)
  ];

  environment.systemPackages = with pkgs; [
    alsaUtils
    hicolor-icon-theme
    rxvt_unicode-with-plugins
    shared-mime-info
    plasma-workspace
    gnome3.zenity

    nix-index
  ];
  programs.dconf.enable = true;
  programs.slock.enable = true;
  environment.pathsToLink = [ "/share" ];

  # font config
  fonts = {
    fontDir.enable = true;
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
		  corefonts  # Micrsoft free fonts
		  unifont # some international languages
      powerline-fonts
      anonymousPro
      emojione
      carlito
      (nerdfonts.override { fonts = [ "AnonymousPro" "DroidSansMono" ]; })
      gyre-fonts
    ];
  };

  # List services that you want to enable
  services.udisks2.enable = true;
  services.devmon.enable = true;

  # audio config
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;

    # low-latency config
    config.pipewire-pulse = {
      "context.properties" = {
        "log.level" = 2;
      };
      "context.modules" = [
        {
          name = "libpipewire-module-rtkit";
          args = {
            "nice.level" = -15;
            "rt.prio" = 88;
            "rt.time.soft" = 200000;
            "rt.time.hard" = 200000;
          };
          flags = [ "ifexists" "nofail" ];
        }
        { name = "libpipewire-module-protocol-native"; }
        { name = "libpipewire-module-client-node"; }
        { name = "libpipewire-module-adapter"; }
        { name = "libpipewire-module-metadata"; }
        {
          name = "libpipewire-module-protocol-pulse";
          args = {
            "pulse.min.req" = "32/48000";
            "pulse.default.req" = "32/48000";
            "pulse.max.req" = "32/48000";
            "pulse.min.quantum" = "32/48000";
            "pulse.max.quantum" = "32/48000";
            "server.address" = [ "unix:native" ];
          };
        }
      ];
      "stream.properties" = {
        "node.latency" = "32/48000";
        "resample.quality" = 1;
      };
    };
    media-session.config.alsa-monitor = {
      rules = [
        {
          matches = [ { "node.name" = "alsa_output.*"; } ];
          actions = {
            update-props = {
              "audio.format" = "S32LE";
              "audio.rate" = 96000; # for USB soundcards it should be twice your desired rate
              "api.alsa.period-size" = 32; # defaults to 1024, tweak by trial-and-error
              #"api.alsa.disable-batch" = true; # generally, USB soundcards use the batch mode
            };
          };
        }
      ];
    };
  };

  # Enable the X11 windowing system.
  services.xserver = {
    autorun = true;
    #autorun = false;
    enable = true;
    
    libinput.enable = true;
    libinput.touchpad = {
      accelProfile = "adaptive";
      accelSpeed = "0.8";
      naturalScrolling = true;
      scrollMethod = "twofinger";
      tapping = false;
    };

    displayManager.lightdm = {
      enable = true;
      extraSeatDefaults = ''
        greeter-show-manual-login = true
        greeter-hide-users = true
        allow-guest = false
        hide-user-image = true
      '';
      #greeters.tiny.enable = true;
      greeters.gtk.enable = true;
      greeters.gtk = {
        theme.package = pkgs.arc-theme;
        theme.name = "Arc-Darker";

        iconTheme.package = pkgs.arc-icon-theme;
        iconTheme.name = "Arc";

        indicators = [ "~spacer" "~host" "~spacer" "~clock" "~session" "~power" ];

        extraConfig = ''
          default-user-image = ${system-icons}/share/icons/64x64/${config.networking.hostName}.png
        '';
      };
    };
    displayManager.defaultSession = "none+xmonad";
    desktopManager.xterm.enable = false;
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
    };

  };
  services.gnome.at-spi2-core.enable = true;
}
