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

  nixpkgs.overlays = [
    (import ../overlays/slock.nix)
  ];

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
    feh
    sxiv
    (callPackage ../pkgs/screenshot {})

    (callPackage ../pkgs/gopass-dmenu.nix {})
    (callPackage ../pkgs/syncthing-quick-status.nix {})

    (haskellPackages.callPackage ../pkgs/zk {}) # notes
    obsidian

    firefox

    okular
    zathura
    breeze-icons # needed for okular
  ];
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
      corefonts
      emojione
      helvetica-neue-lt-std
      carlito
      nerdfonts
    ];
  };

  # picom compositor
  services.picom.enable = true;

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
    #autorun = false;
    enable = true;
    
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

    displayManager.lightdm.enable = true;
    displayManager.defaultSession = "none+xmonad";
    desktopManager.xterm.enable = false;
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
    };

  };
  services.gnome.at-spi2-core.enable = true;
}
