{ config, pkgs, ... }:

{
  nix = {
    package = pkgs.nixUnstable; # or versioned attributes like nix_2_4
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  imports = [
    <home-manager/nixos>
    <agenix/modules/age.nix>
    ../secrets/home-manager.nix
  ];

  # firewall config
  networking.firewall.enable = true;

  environment.systemPackages = with pkgs; [
    nox
    git
    vim
    atool
    file

    (import <agenix> {}).agenix
  ];

  programs.zsh = {
    enable = true;
  };
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
		fontDir.enable = true;
		enableGhostscriptFonts = true;
		fonts = with pkgs; [
		  corefonts  # Micrsoft free fonts
		  unifont # some international languages
      powerline-fonts
      anonymousPro
		];
  };

  nixpkgs.config.allowUnfree = true;

  # set build cores automatically
  nix.buildCores = 0;

  # List services that you want to enable

  # periodically run GC
  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 10d";
  };
  nix.autoOptimiseStore = true;

  # limit the systemd journal so it doesn't just fill up
  services.journald.extraConfig = ''
    SystemMaxUse=100M
    MaxFileSec=7day
  '';

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  hardware.enableAllFirmware = true;

  # keep a backup of the configuration
  system.copySystemConfiguration = true;
}
