# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ../../hardware-configuration.nix
      ./config/base.nix
      ./config/dev.nix
      ./config/writing.nix
      ./config/laptop.nix
      ./config/desktop-full.nix
      ./config/teaching.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.kernelParams = [
    # maybe helps (https://hobo.house/2018/05/18/fix-for-intel-i915-gpu-freeze-on-recent-linux-kernels/)
    #"i915.enable_psr=0"
    # from https://www.ivanov.biz/2019/howto-optimize-intel-graphics-performance-fedora-kde-linux-laptop/
    #"i915.enable_dc=2"
    #"i915.enable_power_well=0"
    #"i915.enable_fbc=1"
    #"i915.enable_guc=3"
    #"i915.enable_dpcd_backlight=1"
  ];
  boot.initrd.luks.devices = [
    {
      name = "root";
      device = "/dev/nvme0n1p3";
      preLVM = true;
      allowDiscards = true;
    }
  ];

  networking.hostName = "philadelphia"; # Define your hostname.
  networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Select internationalisation properties.
  services.xserver.layout = "gb,us";
  services.xserver.videoDrivers = ["intel"];
  #services.xserver.videoDrivers = ["modesetting"];
  #services.xserver.videoDrivers = ["displaylink" "modesetting"];
  i18n.consoleUseXkbConfig = true;

  # Enable intel iris drivers
  environment.variables = {
    MESA_LOADER_DRIVER_OVERRIDE = "iris";
  };
  hardware.opengl = {
    enable = true;
    driSupport32Bit = true;
    extraPackages = [
      pkgs.vaapiIntel pkgs.vaapiVdpau 
      pkgs.libvdpau-va-gl pkgs.intel-media-driver
    ];
    package = (pkgs.mesa.override {
      galliumDrivers = [ "nouveau" "virgl" "swrast" "iris" ];
    }).drivers;
  };
  nixpkgs.config.packageOverrides = pkgs: {
  };

  # Set your time zone.
  #time.timeZone = "Europe/London";

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    samba # for samba printer
    system-config-printer
    restic
  ];

  # List services that you want to enable:

  hardware.bluetooth.enable = false;

  # Enable CUPS to print documents.
  services.printing.enable = true;
  services.printing.drivers = [pkgs.gutenprint pkgs.gutenprintBin];
  #services.printing.logLevel = "debug";

  # set up sleep/hiberante
  services.logind = {
    # FIXME: why doesn't this work!?!?!
    lidSwitch = "hibernate";
    #lidSwitchDocked = "hibernate";
    lidSwitchExternalPower = "hibernate";
    extraConfig = ''
      HandleSuspendKey = hibernate
    '';
  };

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
    extraGroups = [ "wheel" "lp" "lpadmin" ]; # Enable ‘sudo’ for the user.
    createHome = true;
    shell = "/run/current-system/sw/bin/zsh";
  };
  home-manager.users.tom = import ./home-config/desktop.nix; # needs to be a function

  fileSystems = {
    "/mnt/diskstation" = {
      device = "//192.168.0.14/homes";
      fsType = "cifs";
      options = let
        # this line prevents hanging on network split
        automount_opts = "x-systemd.automount,noauto,x-systemd.idle-timeout=60,x-systemd.device-timeout=5s,x-systemd.mount-timeout=10s,file_mode=0660,dir_mode=0770,gid=1,nounix";
      in ["${automount_opts},credentials=/etc/nixos/smb-secrets,vers=1.0"];
    };
  };

  # printers
  hardware.printers.ensurePrinters = [
    {
      name = "Swansea";
      description = "Swansea Uni Printers";
      deviceUri = "smb://iss-ricoh-ps4.tawe.swan.ac.uk/staff%20printing";
      ppdOptions = {PageSize = "A4";};
      model = "gutenprint.5.2://ricoh-mp_c5503/expert";
    }
  ];
  nixpkgs.overlays = [
  ];

  # home backup
  services.restic.backups = {
    local = {
      paths = [ "/home" ];
      repository = "sftp:backup@192.168.0.14:/backup";
      passwordFile = "/etc/nixos/secrets/restic-password";
      extraBackupArgs = [
        "--exclude='home/*/.cache'"
        "--exclude='home/*/.mozilla'"
      ];
      extraOptions = [
        "sftp.command='ssh backup@192.168.0.14 -i /etc/nixos/secrets/diskstation.rsa -s sftp'"
      ];
      timerConfig = {
        OnBootSec = "2m";
        OnUnitInactiveSec = "1d";
      };
    };
    remote = {
      paths = [ "/home" ];
      repository = "s3:https://s3.eu-central-1.wasabisys.com/gabysbrain-restic";
      passwordFile = "/etc/nixos/secrets/restic-password";
      s3CredentialsFile = "/etc/nixos/secrets/wasabi";
      extraBackupArgs = [
        "--exclude='home/*/.cache'"
        "--exclude='home/*/.mozilla'"
      ];
      timerConfig = {
        OnBootSec = "2m";
        OnUnitInactiveSec = "1d";
      };
    };
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.03"; # Did you read the comment?

}
