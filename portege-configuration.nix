# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

let nasMount = remotePath: {
      device = "//diskstation.lan/${remotePath}";
      fsType = "cifs";
      options = let
        # this line prevents hanging on network split
        automount_opts = "x-systemd.automount,noauto,x-systemd.idle-timeout=60,x-systemd.device-timeout=5s,x-systemd.mount-timeout=10s,file_mode=0660,dir_mode=0770,gid=1,nounix";
      in ["${automount_opts},credentials=/run/agenix/diskstation-smb,vers=1.0"];
    };
in
{
  imports =
    [ # Include the results of the hardware scan.
      ../hardware-configuration.nix
      ./nixos/common.nix
      ./nixos/laptop.nix
      ./nixos/desktop.nix
      ./nixos/vrvis.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.extraModprobeConfig = ''
    "options iwlwifi 11n_disable=1"
    "options iwlwifi swcrypto=0"
    "options iwlwifi bt_coex_active=0"
    "options iwlwifi power_save=0"
    "options iwlwifi uapsd_disable=1"
    "options iwlmvm power_scheme=1"
  '';

  # for building nixos on other systems (e.g. raspberry pi)
  boot.binfmt.emulatedSystems = [ "aarch64-linux" ];

  # since I lock down my data drive
  boot.initrd.luks.devices = {
    root = {
      device = "/dev/nvme0n1p2";
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
  services.xserver.xkbOptions = "grp:win_caps_toggle,compose:menu,terminate:ctrl_alt_bksp";
  console.useXkbConfig = true;

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
    extraGroups = [ "wheel" "lp" "lpadmin" "adbusers" ]; # Enable ‘sudo’ for the user.
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
    ];
  };

  age.secrets.diskstation-smb.file = ./secrets/diskstation-smb.age;
  fileSystems."/mnt/diskstation" = nasMount "homes";
  fileSystems."/mnt/media/videos" = nasMount "videos";
  fileSystems."/mnt/media/music" = nasMount "music";

  # home backup
  age.secrets.wasabi.file = ./secrets/wasabi.age;
  age.secrets.restic.file = ./secrets/restic.age;
  age.secrets.diskstation-key.file = ./secrets/diskstation-key.age;
  services.restic.backups = {
    local = {
      paths = [ "/home" ];
      repository = "sftp:backup@diskstation.lan:/backup";
      passwordFile = "/run/agenix/restic"; # FIXME: this should use age.secrets.restic.path somehow
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
        "sftp.command='ssh backup@diskstation.lan -i /run/agenix/diskstation-key -s sftp'"
      ];
      timerConfig = {
        OnBootSec = "2m";
        OnUnitInactiveSec = "1d";
      };
    };
    remote = {
      paths = [ "/home" ];
      repository = "s3:https://s3.eu-central-1.wasabisys.com/gabysbrain-restic";
      passwordFile = "/run/agenix/restic"; # FIXME: this should use age.secrets.restic.path somehow
      s3CredentialsFile = "/run/agenix/wasabi"; # FIXME: this should use age.secrets.wasbi.path
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

  # virtualbox
  virtualisation.virtualbox.host.enable = true;
  virtualisation.virtualbox.host.enableExtensionPack = true;
  users.extraGroups.vboxusers.members = [ "tom" ];

  # android dev stuff
  programs.adb.enable = true;

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.03"; # Did you read the comment?

}
