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

  environment.systemPackages = with pkgs; [
    alsaUtils
    hicolor-icon-theme
    rxvt_unicode-with-plugins
    shared-mime-info
    plasma-workspace
    gnome3.zenity
    usbutils
    pciutils

    nix-index
  ];
  programs.dconf.enable = true;
  programs.slock.enable = true;
  environment.pathsToLink = [ "/share" ];

  # font config
  fonts = {
    fontDir.enable = true;
    enableGhostscriptFonts = true;
    packages = with pkgs; [
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

    wireplumber.enable = true;
  };

  # touchpad stuff
  services.libinput = {
    enable = true;
    touchpad = {
      accelProfile = "adaptive";
      accelSpeed = "0.8";
      naturalScrolling = true;
      scrollMethod = "twofinger";
      tapping = false;
    };
  };

  # Enable the X11 windowing system.
  services.xserver = {
    autorun = true;
    #autorun = false;
    enable = true;
    
    desktopManager.xterm.enable = false;
    displayManager.startx.enable = true;
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
    };

  };

  services.greetd = {
    enable = true;
    settings = {
      default_session = {
        command = builtins.concatStringsSep " " [
          "${pkgs.greetd.tuigreet}/bin/tuigreet"
          "--time"
          "--cmd startx"
        ];
        user = "greeter";
      };
      switch = false;
    };
  };

  # hat tip: https://www.reddit.com/r/NixOS/comments/u0cdpi/tuigreet_with_xmonad_how/
  systemd.services.greetd.serviceConfig = {
    type = "idle";
    StandardInput = "tty";
    StandardOutput = "tty";
    StandardError = "journal"; # Without this errors will spam on screen
    # Without these bootlogs will spam on screen
    TTYReset = true;
    TTYVHangup = true;
    TTYVTDisallocate = true;
  };

  services.gnome.at-spi2-core.enable = true;
  services.displayManager.defaultSession = "none+xmonad";
}
