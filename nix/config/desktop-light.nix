{ config, pkgs, ... }:

let 
  vdirsyncer-patched = pkgs.callPackage ../pkgs/vdirsyncer-patched.nix {};
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
    chromium = {
      enablePepperFlash = false;
    };
  };
  environment.systemPackages = with pkgs; [
    alsaUtils
    gnome3.dconf
    hicolor-icon-theme
    rxvt_unicode-with-plugins
    shared_mime_info
    kinit
    plasma-workspace
    haskellPackages.xmobar
    dmenu
    gnome3.zenity

    xclip
    xdotool
    lf
    feh
    sxiv
    mutt
    offlineimap
    msmtp
    vdirsyncer-patched
    (callPackage ../pkgs/terminal-velocity.nix {})
    gcalcli
    python37Packages.goobook
    urlscan
    notmuch
    notmuch-mutt
    w3m

    #dropbox-cli

    (callPackage ../pkgs/gopass-dmenu.nix {})
    (callPackage ../pkgs/syncthing-quick-status.nix {})

    firefox
    browserpass

    okular
    zathura
    breeze-icons # needed for okular
  ];
  environment.pathsToLink = [ "/share" ];

  fonts = {
    enableFontDir = true;
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
      corefonts
      emojione
      helvetica-neue-lt-std
      carlito
      nerdfonts
    ];
  };

  # List services that you want to enable

  # Use browserpass
  programs.browserpass.enable = true;

  # Enable the X11 windowing system.
  services.xserver = {
    autorun = true;
    enable = true;
    #layout = "us";
    
    # use compose key and switch layouts with caps lock
    xkbOptions = "grp:caps_toggle,compose:menu";

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
