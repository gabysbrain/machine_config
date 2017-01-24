# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot = {
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;
    #blacklistedKernelModules =[ "i2c_designware_core" "i2c_designware_platform" ];
    kernelPackages = pkgs.linuxPackages_4_8;
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
      enablePepperFlash = true;
    };
  };
  environment.systemPackages = with pkgs; [
    nox
    chromium
    vim
    git
    dmenu
    networkmanagerapplet
    gnome3.dconf
    rxvt_unicode-with-plugins

    cabal-install
    ghc
    stack
    nodejs
    haskellPackages.purescript

    gnumake

    git
    silver-searcher
    mutt
    rxvt_unicode-with-plugins
    #myvim
    #zshrc
    dropbox-cli
    evince
    dzen2
    conky
    #texlive-combined-full-2016
  ];
  programs.zsh = {
    enable = true;
    interactiveShellInit = ''
      cat << EOF > $HOME/.zshrc
        . ${import /home/tom/Projects/dotfiles/nix/zsh-config.nix}
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

  # List services that you want to enable:

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
    synaptics = {
      enable = true;
      twoFingerScroll = true;
    };

    # Enable slim with xmonad
    displayManager = {
      #lightdm.enable = true;
      #sddm.enable = true;
      slim.enable = true;
    };
    #desktopManager.kde5.enable = true;
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

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "16.09";
}
