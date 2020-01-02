{ config, pkgs, ... }:

let 
  home-manager = builtins.fetchGit {
    url = "https://github.com/rycee/home-manager.git";
    rev = "af2303526d425b9aed15134f28efdf29b038daaf"; # CHANGEME 
    ref = "release-19.09";
  };
in
{
  imports = [
    # home-manager
    #"${builtins.fetchTarball https://github.com/rycee/home-manager/archive/master.tar.gz}/nixos"
    "${home-manager}/nixos"
  ];
  networking = {
    # firewall config
    firewall.enable = true;
  };

  environment.systemPackages = with pkgs; [
    nox
    git
    silver-searcher
    unzip
    nvi
    atool
    p7zip
    file
    mimeo
    peco

    # password storage
    gopass
    gnupg
  ];

  programs.zsh = {
    enable = true;
  };
	
  services.syncthing = {
    enable = true;
    #user = "tom";
    #dataDir = "/home/tom/.config/syncthing";
    openDefaultPorts = true;
  };

  # figure out correct time zone
  services.localtime.enable = true; 

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

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  hardware.enableAllFirmware = true;

  # keep a backup of the configuration
  system.copySystemConfiguration = true;
}
