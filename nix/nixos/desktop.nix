{ config, pkgs, ... }:

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
    gnome3.dconf
    hicolor-icon-theme
    rxvt_unicode-with-plugins
    shared_mime_info
    plasma-workspace
    haskellPackages.xmobar
    dmenu
    gnome3.zenity

    nix-index

    silver-searcher
    unzip
    unrar
    p7zip
    mimeo
    peco

    xclip
    xdotool
    lf
    feh
    sxiv
    (callPackage ../pkgs/screenshot {})

    # PIM stuff
    alot
    msmtp
    vdirsyncer
    khal
    khard
    urlscan
    notmuch
    afew
    gmailieer
    w3m

    (callPackage ../pkgs/gopass-dmenu.nix {})
    (callPackage ../pkgs/syncthing-quick-status.nix {})

    (haskellPackages.callPackage ../pkgs/zk {}) # notes
    obsidian

    firefox

    okular
    zathura
    breeze-icons # needed for okular
  ];
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
      corefonts
      emojione
      helvetica-neue-lt-std
      carlito
      nerdfonts
    ];
  };

  # List services that you want to enable
  services.udisks2.enable = true;
  services.devmon.enable = true;

  # audio config
  hardware.pulseaudio = {
    enable = true;
    daemon.config = {
      flat-volumes = "no";
      default-sample-format = "s24le";
      default-sample-rate = "192000";
      resample-method = "speex-float-10";
      avoid-resampling = "true";
    };
    package = pkgs.pulseaudioFull;
  };
  nixpkgs.config.pulseaudio = true;

  # Enable the X11 windowing system.
  services.xserver = {
    autorun = true;
    enable = true;
    #layout = "us";
    
    # use compose key and switch layouts with caps lock
    xkbOptions = "grp:caps_toggle,compose:menu";

    libinput.enable = true;
    libinput.touchpad = {
      accelProfile = "adaptive";
      accelSpeed = "0.8";
      naturalScrolling = true;
      scrollMethod = "twofinger";
      tapping = false;
    };

    # Enable lightdm with xmonad
    displayManager = {
      lightdm.enable = true;

      #sddm.enable = true;
      sessionCommands = "~/.xmonad/xmonad-session-rc";
    };
    /*
    displayManager ={
      startx.enable = true;
    };
    */

    desktopManager.xterm.enable = false;
    displayManager.defaultSession = "none+xmonad";
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
    };

  };
  services.gnome.at-spi2-core.enable = true;
}
