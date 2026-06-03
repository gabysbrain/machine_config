{ config, pkgs, ... }:

{
  nix.settings.experimental-features = [
    "nix-command"
    "flakes"
  ];
  nix.settings.trusted-users = [ "@wheel" ];
  nix.settings.trusted-substituters = [ "https://cachix.joukamachi.net/prod" ];
  nix.settings.trusted-public-keys = [
    "prod:YvdQaSxvCua1bSMOD3JQj7eexVTZhmeHWWY842+T+aM="
  ];

  # make sure tmp directory gets cleaned
  boot.tmp.cleanOnBoot = true;

  # firewall config
  networking.firewall.enable = true;

  environment.systemPackages = with pkgs; [
    nox
    nix-output-monitor
    git
    vim
    atool
    file
    dig

    agenix.packages.x86_64-linux.default
  ];

  programs.zsh = {
    enable = true;
  };

  # hack to allow certain programs to run
  programs.nix-ld.enable = true;
  #programs.gnupg.agent.enable = true;

  services.syncthing = {
    enable = true;
    #user = "tom";
    #dataDir = "/home/tom/.config/syncthing";
    openDefaultPorts = true;
  };

  # figure out correct time zone
  # FIXME: this is currently broken in nixos: https://github.com/NixOS/nixpkgs/issues/68489
  #services.localtime.enable = true;

  # font config
  fonts = {
    packages = with pkgs; [
      corefonts # Micrsoft free fonts
      unifont # some international languages
      powerline-fonts
      anonymousPro
    ];
  };

  nixpkgs.config.allowUnfree = true;

  # set build cores automatically
  nix.settings.cores = 0;

  # List services that you want to enable

  # limit the systemd journal so it doesn't just fill up
  services.journald.extraConfig = ''
    SystemMaxUse=100M
    MaxFileSec=7day
  '';

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # auto clean direnv and old profiles
  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 10d";
  };
  nix.settings.auto-optimise-store = true;

  # mostly for direnv profiles
  services.angrr = {
    enable = true;
    enableNixGcIntegration = true;
    timer.enable = true;
    timer.dates = "daily";

    settings = {
      profile-policies = {
        system = {
          keep-booted-system = true;
          keep-current-system = true;
          #keep-latest-n = 5;
          keep-since = "10d";
          profile-paths = [
            "/nix/var/nix/profiles/system"
          ];
        };
        user = {
          enable = true;
          keep-booted-system = true;
          keep-current-system = true;
          #keep-latest-n = 1;
          keep-since = "10d";
          profile-paths = [
            "~/.local/state/nix/profiles/profile"
            "/nix/var/nix/profiles/per-user/root/profile"
          ];
        };
      };
      temporary-root-policies = {
        direnv = {
          path-regex = "/\\.direnv/";
          period = "14d";
        };
        result = {
          path-regex = "/result[^/]*$";
          period = "3d";
        };
      };
    };
  };
  programs.direnv.enable = true;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  hardware.enableAllFirmware = true;
}
