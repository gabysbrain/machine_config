# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ../../hardware-configuration.nix
      ./nixos/common.nix
      ./nixos/laptop.nix
      ./nixos/desktop.nix
      ./nixos/vrvis.nix
      ./nixos/games.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;


  # for building nixos on other systems (e.g. raspberry pi)
  boot.binfmt.emulatedSystems = [ "aarch64-linux" ];

  boot.initrd.luks.devices = {
    root = {
      device = "/dev/nvme0n1p4";
      preLVM = true;
      allowDiscards = true;
    };
  };
  boot.supportedFilesystems = [ "ntfs" ];

  networking.hostName = "philadelphia"; # Define your hostname.
  networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.wireless.interfaces = [ "wlp2s0" ];

  # Select internationalisation properties.
  services.xserver.layout = "gb,us";
  services.xserver.exportConfiguration = true;
  console.useXkbConfig = true;

  #hardware.openrazer.enable = true;

  # Video drivers setup
  services.xserver.videoDrivers = [ "intel" ];

  hardware.opengl = {
    enable = true;
    extraPackages = with pkgs; [
      intel-media-driver # LIBVA_DRIVER_NAME=iHD
      vaapiIntel         # LIBVA_DRIVER_NAME=i965 (older but works better for Firefox/Chromium)
      vaapiVdpau
      libvdpau-va-gl
    ];
    driSupport32Bit = true;
  };

  # User level thunderbolt 3 drivers
  services.hardware.bolt.enable = true;

  nixpkgs.config.packageOverrides = pkgs: {
    vaapiIntel = pkgs.vaapiIntel.override { enableHybridCodec = true; };
  };

  # Set your time zone.
  time.timeZone = "Europe/Vienna";
  #services.localtime.enable = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    restic

    thunderbolt
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
    isNormalUser = true;
  };
  home-manager.users.tom = { pkgs, ... }: {
    imports = [
      ./home-config/common.nix
      ./home-config/desktop.nix
      ./profiles/dev.nix
      ./profiles/writing.nix
      ./profiles/games.nix
      ./profiles/vrvis.nix
    ];
  };

  fileSystems = {
    "/mnt/diskstation" = {
      device = "//192.168.0.14/homes";
      fsType = "cifs";
      options = let
        # this line prevents hanging on network split
        automount_opts = "x-systemd.automount,noauto,x-systemd.idle-timeout=60,x-systemd.device-timeout=5s,x-systemd.mount-timeout=10s,file_mode=0660,dir_mode=0770,gid=1,nounix";
      in ["${automount_opts},credentials=/etc/nixos/smb-secrets,vers=1.0"];
    };
    "/mnt/media/videos" = {
      device = "//192.168.0.14/videos";
      fsType = "cifs";
      options = let
        # this line prevents hanging on network split
        automount_opts = "x-systemd.automount,noauto,x-systemd.idle-timeout=60,x-systemd.device-timeout=5s,x-systemd.mount-timeout=10s,file_mode=0660,dir_mode=0770,gid=1,nounix";
      in ["${automount_opts},credentials=/etc/nixos/smb-secrets,vers=1.0"];
    };
    "/mnt/media/music" = {
      device = "//192.168.0.14/music";
      fsType = "cifs";
      options = let
        # this line prevents hanging on network split
        automount_opts = "x-systemd.automount,noauto,x-systemd.idle-timeout=60,x-systemd.device-timeout=5s,x-systemd.mount-timeout=10s,file_mode=0660,dir_mode=0770,gid=1,nounix";
      in ["${automount_opts},credentials=/etc/nixos/smb-secrets,vers=1.0"];
    };
  };

  # home backup
  services.restic.backups = {
    local = {
      paths = [ "/home" ];
      repository = "sftp:backup@192.168.0.14:/backup";
      passwordFile = "/etc/nixos/secrets/restic-password";
      extraBackupArgs = [
        "--exclude='home/tom/Downloads'"
        "--exclude='home/tom/Sync'"
        "--exclude='home/*/.cache'"
        "--exclude='home/*/.config'"
        "--exclude='home/*/.local'"
        "--exclude='home/*/.mozilla'"
      ];
      pruneOpts = [
        "--keep-daily 7"
        "--keep-weekly 8"
        "--keep-monthly 24"
        "--keep-yearly 5"
        "--keep-last 2"
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
        "--exclude='home/tom/Downloads'"
        "--exclude='home/tom/Sync'"
        "--exclude='home/*/.cache'"
        "--exclude='home/*/.mozilla'"
        "--exclude='home/*/.julia'"
        "--exclude='home/*/.local'"
      ];
      pruneOpts = [
        "--keep-daily 7"
        "--keep-weekly 8"
        "--keep-monthly 24"
        "--keep-yearly 5"
        "--keep-last 2"
      ];
      timerConfig = {
        OnBootSec = "2m";
        OnUnitInactiveSec = "1d";
      };
    };
  };

  # send restic logs for prometheus
  systemd.services.restic-backups-remote-metrics = {
    description = "Generate prometheus metrics from restic";
    wantedBy = [ "multi-user.target" ];

    #preStart = "mkdir -p /metrics";
    environment = {
      RESTIC_PASSWORD_FILE = "/etc/nixos/secrets/restic-password";
      RESTIC_REPOSITORY = "s3:https://s3.wasabisys.com/gabysbrain-restic";
      RESTIC_CACHE_DIR = "/root/.cache/restic";
    };
    serviceConfig = {
      ExecStart = ''
        ${pkgs.callPackage ./pkgs/restic-metrics {}}/bin/restic-metrics
      '';
      EnvironmentFile = "/etc/nixos/secrets/wasabi";
    };
  };
  systemd.timers.restic-backups-remote-metrics = {
    description = "Regenerate restic prometheus metrics";
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnBootSec = "5m";
      OnUnitInactiveSec = "24h"; 
    };
  };

  # promtail to get logs into loki
  systemd.services.promtail = {
    description = "Promtail service for Loki";
    wantedBy = [ "multi-user.target" ];

    serviceConfig = {
      ExecStart = ''
        ${pkgs.grafana-loki}/bin/promtail --config.file ${./promtail.yaml}
      '';
    };
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.03"; # Did you read the comment?

}
