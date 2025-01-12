# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, lib, pkgs, ... }:

{
  imports =
    [ #./nixos/nvidia-fix.nix
      ./nixos/common.nix
      ./nixos/games.nix
      ./nixos/desktop.nix
      ./nixos/rgb.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # for building nixos on other systems (e.g. raspberry pi)
  boot.binfmt.emulatedSystems = [ "aarch64-linux" ];

  boot.initrd.availableKernelModules = [ "nvme" "xhci_pci" "ahci" "usb_storage" "usbhid" "sd_mod" ];
  boot.initrd.kernelModules = [ "dm-snapshot" "dm-mirror" ];
  boot.kernelModules = [ "kvm-amd" ];
  boot.extraModulePackages = [ ];
  boot.extraModprobeConfig = ''
    options snd_usb_audio vid=0x1235 pid=0x8201 device_setup=1
  '';

  fileSystems."/" =
    { device = "/dev/disk/by-label/nixos";
      fsType = "ext4";
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-label/boot";
      fsType = "vfat";
    };

  fileSystems."/home" =
    { device = "/dev/disk/by-label/home";
      fsType = "ext4";
    };

  fileSystems."/games" =
    { device = "/dev/disk/by-label/games";
      fsType = "ext4";
    };

  swapDevices =
    [ { device = "/dev/disk/by-label/swap"; }
    ];

  hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

  networking.hostName = "katana"; # Define your hostname.

  # Disable roccat tyon working as a joystick
  services.udev.extraRules = ''
  SUBSYSTEM=="usb", ATTR{bInterfaceNumber}=="02", ATTRS{idVendor}=="1e7d", ATTRS{idProduct}=="2e4a|2e4b", RUN+="${pkgs.busybox}/bin/sh -c '${pkgs.busybox}/bin/echo -n %k >/sys$${DEVPATH}/driver/unbind'"
  '';

  # Video drivers setup
  services.xserver.videoDrivers = [ "nvidia" ];
  hardware.graphics = {
    enable = true;
    enable32Bit = true;
  };
  hardware.nvidia = {
    # Modesetting is required.
    modesetting.enable = true;

    # Nvidia power management. Experimental, and can cause sleep/suspend to fail.
    # Enable this if you have graphical corruption issues or application crashes after waking
    # up from sleep. This fixes it by saving the entire VRAM memory to /tmp/ instead 
    # of just the bare essentials.
    powerManagement.enable = false;

    # Fine-grained power management. Turns off GPU when not in use.
    # Experimental and only works on modern Nvidia GPUs (Turing or newer).
    powerManagement.finegrained = false;

    # Use the NVidia open source kernel module (not to be confused with the
    # independent third-party "nouveau" open source driver).
    # Support is limited to the Turing and later architectures. Full list of 
    # supported GPUs is at: 
    # https://github.com/NVIDIA/open-gpu-kernel-modules#compatible-gpus 
    # Only available from driver 515.43.04+
    open = true;

    # Enable the Nvidia settings menu,
	  # accessible via `nvidia-settings`.
    nvidiaSettings = true;
  };

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.enp5s0.useDHCP = true;
  #networking.interfaces.wlp4s0.useDHCP = true;

  # Set your time zone.
  time.timeZone = "Europe/Vienna";
  #services.localtime.enable = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    restic
    alsa-scarlett-gui
  ];

  ### List services that you want to enable:

  # this machine uses 27 inch 4k displays
  services.xserver.dpi = 157; # got this off the internet...

  # allow nvidia overclocking and overvolting (8 + 16 respectively)
  services.xserver.deviceSection = ''
    Option "Coolbits" "24"
  '';

  # The remaining syncthing config
  services.syncthing = {
    enable = true;
    user = "tom";
    dataDir = "/home/tom/";
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.tom = {
    home = "/home/tom";
    description = "Thomas Torsney-Weir";
    extraGroups = [ "wheel" "lp" "lpadmin" "adbusers" ]; # Enable ‘sudo’ for the user.
    createHome = true;
    shell = "/run/current-system/sw/bin/zsh";
    isNormalUser = true;
  };

  # home backup
  age.secrets.restic.file = ./secrets/restic.age;
  services.restic.backups = {
    local = {
      paths = [ "/home" ];
      repository = "rest:https://backup.joukamachi.net/";
      passwordFile = "/run/agenix/restic"; # FIXME: this should use age.secrets.restic.path somehow
      exclude = [
        "**/.cache"
        "**/cache"
        "home/**/Downloads"
        "home/**/Sync"
        "home/*/.cache"
        "home/*/.config"
        "home/*/.julia"
        "home/*/.local"
        "home/*/.mozilla"
        "home/tom/raicoon"
      ];
      extraBackupArgs = [
      ];
      timerConfig = {
        OnBootSec = "2m";
        OnUnitInactiveSec = "1d";
      };
    };
  };

  # Enable CUPS to print documents.
  services.printing.enable = true;
  #services.avahi.enable = true;
  #services.avahi.nssmdns = true;
  #services.avahi.openFirewall = true;
  services.printing.drivers = [ pkgs.brlaser ];

  #services.openvpn.servers = {
    #raicoon = { 
      #config = '' config /home/tom/VPN_GTS-a5d93c92a9bede02703f3e528b5030fb.ovpn ''; 
      #autoStart = false;
      ##updateResolvConf = true;
    #};
  #};

  # virtualization
  virtualisation.virtualbox.host.enable = true;
  virtualisation.virtualbox.host.enableExtensionPack = true;
  users.extraGroups.vboxusers.members = [ "tom" ];

  virtualisation.docker.enable = true;
  users.extraGroups.docker.members = [ "tom" ];

  # LLM stuff
  services.ollama = {
    enable = true;
    acceleration = "cuda";
  };

  # android dev stuff
  programs.adb.enable = true;

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "21.01"; # Did you read the comment?

}
