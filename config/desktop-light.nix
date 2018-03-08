{ config, pkgs, ... }:

{
  networking = {
    wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  };

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
    chromium = {
      enablePepperFlash = false;
    };
  };
  environment.systemPackages = with pkgs; [
    alsaUtils
    gnome3.dconf
    rxvt_unicode-with-plugins
    shared_mime_info
    kinit
    plasma-workspace
    haskellPackages.xmobar
    dzen2
    conky
    dmenu

    feh
    mutt
    offlineimap
    msmtp
    (import ../pkgs/vdirsyncer.nix)
    (import ../pkgs/terminal-velocity.nix)
    khard
    khal
    urlscan
    notmuch
    notmuch-mutt
    w3m

    #dropbox-cli
    owncloud-client

    chromium
    browserpass

    okular
  ];
  environment.pathsToLink = [ "/share" ];

  fonts = {
    enableFontDir = true;
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
      corefonts
      emojione
    ];
  };

  # List services that you want to enable

  # Enable the X11 windowing system.
  services.xserver = {
    autorun = true;
    enable = true;
    layout = "us";
    #xkbOptions = "eurosign:e";

    # synaptics
    #synaptics = {
      #enable = true;
      #twoFingerScroll = true;
    #};
    libinput = {
      enable = true;
      accelProfile = "adaptive";
      accelSpeed = "0.8";
      naturalScrolling = true;
      scrollMethod = "twofinger";
      tapping = false;
    };

    # Enable slim with xmonad
    displayManager = {
      lightdm = {
        enable = true;
      };
      #sddm.enable = true;
      sessionCommands = "~/.xmonad/xmonad-session-rc";
    };

    desktopManager.xterm.enable = false;
    desktopManager.default = "none";
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
    };
    windowManager.default = "xmonad";
  };
  services.gnome3.at-spi2-core.enable = true;
}
