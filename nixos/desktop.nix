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
    shared_mime_info
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
