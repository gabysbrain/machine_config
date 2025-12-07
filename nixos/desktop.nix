{ config, pkgs, ... }:

{
  # Select internationalisation properties.
  # i18n = {
  #   consoleFont = "Lat2-Terminus16";
  #   consoleKeyMap = "us";
  #   defaultLocale = "en_US.UTF-8";
  # };

  console.packages = [ pkgs.terminus_font ];
  # see https://files.ax86.net/terminus-ttf/README.Terminus.txt for font path meaning
  console.font = "${pkgs.terminus_font}/share/consolefonts/ter-i14n.psf.gz";
  console.colors = [
    "282828" # color0
    "cc241d" # color1
    "98971a" # color2
    "d79921" # color3
    "458588" # color4
    "b16286" # color5
    "689d6a" # color6
    "a89984" # color7
    "928374" # color8
    "fb4934" # color9
    "b8bb26" # color10
    "fabd2f" # color11
    "83a598" # color12
    "d3869b" # color13
    "8ec07c" # color14
    "ebdbb2" # color15
  ];


  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  nixpkgs.config = {
    allowUnfree = true;
  };

  environment.systemPackages = with pkgs; [
    alsa-utils
    hicolor-icon-theme
    shared-mime-info
    kdePackages.plasma-workspace
    zenity
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
      #anonymousPro
      carlito
      nerd-fonts.anonymice
      nerd-fonts.droid-sans-mono
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
        # FIXME: ideally this would use the xsession stuff and not hack xinitrc
        command = builtins.concatStringsSep " " [
          "${pkgs.tuigreet}/bin/tuigreet"
          "--theme 'border=gray;text=green;prompt=red;time=purple;action=blue;button=yellow;container=black;input=white'"
          "--window-padding 1"
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
