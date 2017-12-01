# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

  #myvim = import /home/tom/Projects/dotfiles/nix/vim.nix;
{
  imports =
    [ # Include the results of the hardware scan.
      ../hardware-configuration.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot = {
    loader = {
      grub.splashImage = "${pkgs.nixos-artwork}/common/grub2-background/grub-nixos-3.png";
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
    #blacklistedKernelModules =[ "i2c_designware_core" "i2c_designware_platform" ];
    kernelPackages = pkgs.linuxPackages;
    kernelModules = [
      "hid-sensor-trigger"
      "hid-sensor-hub"
      "hid-sensor-als"
      "hid-sensor-accel-3d"
      "hid-sensor-gyro-3d"
      "hid-sensor-magn-3d"
      "hid-sensor-incl-3d"
    ];
  };

  networking = {
    hostName = "nixos"; # Define your hostname.
    networkmanager.enable = true;  # Enables wireless support via wpa_supplicant.
    # firewall config
    firewall.enable = true;
  };

  # Select internationalisation properties.
  # i18n = {
  #   consoleFont = "Lat2-Terminus16";
  #   consoleKeyMap = "us";
  #   defaultLocale = "en_US.UTF-8";
  # };

  # Set your time zone.
  time.timeZone = "Europe/Vienna"; # hmoe time zone
  #time.timeZone = "US/Eastern"; # alternate time zone

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  nixpkgs.config = {
    allowUnfree = true;
    chromium = {
      enablePepperFlash = false;
    };
  };
  environment.systemPackages = with pkgs; [
    nox
    chromium
    #vim
    git
    dmenu
    networkmanagerapplet
    alsaUtils
    gnome3.dconf
    rxvt_unicode-with-plugins

    cabal-install
    ghc
    stack
    nodejs

    gnumake

    haskellPackages.xmobar
    nitrogen
    git
    silver-searcher
    mutt
    offlineimap
    msmtp
    rxvt_unicode-with-plugins
    (import ./vim.nix)
    #myvim
    #zshrc
    dropbox-cli
    owncloud-client
    evince
    dzen2
    conky
    texlive.combined.scheme-full
    bibtool
    pandoc
  ];
  programs.zsh = {
    enable = true;
    interactiveShellInit = ''
      cat << EOF > $HOME/.zshrc
        . ${import ./zsh-config.nix}
      EOF
    '';
  };
	
  # font config
  fonts = {
		enableFontDir = true;
		enableGhostscriptFonts = true;
		fonts = with pkgs; [
		  corefonts  # Micrsoft free fonts
		  unifont # some international languages
      powerline-fonts
      anonymousPro
		];
  };

  # List services that you want to enable

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  hardware.enableAllFirmware = true;

  # Enable the X11 windowing system.
  services.xserver = {
    autorun = true;
    enable = true;
    layout = "us";
    xkbOptions = "eurosign:e";

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

  # disable various buttons, like the power key!
  services.logind.extraConfig = ''
    HandlePowerKey = ignore
  '';

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.tom = {
    name = "tom";
    group = "users";
    extraGroups = [
      "wheel" "disk" "audio" "video" "networkmanager" "systemd-journal"
    ];
    createHome = true;
    uid = 1000;
    home = "/home/tom";
    shell = "/run/current-system/sw/bin/zsh";
  };

  # keep a backup of the configuration
  system.copySystemConfiguration = true;

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "17.03";
}
