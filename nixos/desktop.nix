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
        theme.package = pkgs.juno-theme;
        theme.name = "Juno";

        iconTheme.package = pkgs.flat-remix-icon-theme;
        iconTheme.name = "Flat-Remix-Green-Dark";

        indicators = [ "~spacer" "~host" "~spacer" "~clock" "~session" "~power" ];

        extraConfig = ''
          default-user-image = ${system-icons}/share/icons/64x64/${config.networking.hostName}.png
        '';
      };
    };
    desktopManager.xterm.enable = false;
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
    };

  };
  services.gnome.at-spi2-core.enable = true;
  services.displayManager.defaultSession = "none+xmonad";
}
