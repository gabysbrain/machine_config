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
  #boot.loader.grub = {
    #enable = true;
    #version = 2;
    #device = "nodev";
    #efiSupport = true;
    #canTouchEfiVariables = true;
  #};
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
  i18n.consoleUseXkbConfig = true;

  # Set your time zone.
  #time.timeZone = "Europe/London";

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    samba # for samba printer
  ];

  # List services that you want to enable:

  hardware.bluetooth.enable = false;

  # Enable CUPS to print documents.
  services.printing.enable = true;
  services.printing.drivers = [pkgs.gutenprint pkgs.gutenprintBin];

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
    extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
    createHome = true;
    shell = "/run/current-system/sw/bin/zsh";
  };

  fileSystems."/mnt/diskstation" = {
    device = "//192.168.0.14/homes";
    fsType = "cifs";
    options = let
      # this line prevents hanging on network split
      automount_opts = "x-systemd.automount,noauto,x-systemd.idle-timeout=60,x-systemd.device-timeout=5s,x-systemd.mount-timeout=10s,file_mode=0660,dir_mode=0770,gid=1,nounix";
    in ["${automount_opts},credentials=/etc/nixos/smb-secrets,vers=1.0"];
  };

  # printers
  hardware.printers.ensurePrinters = [
    {
      name = "Swanea";
      description = "Swansea Uni Printers";
      deviceUri = "smb://iss-ricoh-ps4.tawe.swan.ac.uk/Staff%20Printing";
      ppdOptions = {PageSize = "A4";};
      model = "gutenprint.5.2://ricoh-mp_c5503/expert";
    }
  ];

  # home backup
  services.borgbackup.jobs = {
    homeBackup = {
      paths = "/";
      repo = "/mnt/diskstation/gabysbrain/backups/portege";
      compression = "auto,lzma";
      encryption.mode = "none";
      startAt = "daily";
      exclude = [
        "/home/*/.cache"
        "/bin"
        "/boot"
        "/dev"
        "/lost+found"
        "/nix"
        "/mnt"
        "/proc"
        "/run"
        "/sys"
        "/tmp"
        "/usr"
        "/var/cache"
        "/var/lib"
        "/var/run"
        "/var/tmp"
      ];
      prune.keep = {
        within = "1d"; # Keep all archives from the last day
        daily = 7;
        weekly = 4;
        monthly = 12;  # Keep at one archive/month from the last year
        yearly = -1; # Keep at least one archive per year
      };
    };
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.03"; # Did you read the comment?

}
