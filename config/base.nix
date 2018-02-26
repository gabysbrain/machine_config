{ config, pkgs, ... }:

{
  networking = {
    # firewall config
    firewall.enable = true;
  };

  environment.systemPackages = with pkgs; [
    nox
    git
    silver-searcher
    unzip
    (import ../pkgs/vim.nix)

    # password storage
    pass
    gnupg
  ];

  programs.zsh = {
    enable = true;
  };
	
  services.syncthing = {
    enable = true;
    user = "tom";
    dataDir = "/home/tom/.config/syncthing";
    openDefaultPorts = true;
  };

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

  # List services that you want to enable

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  hardware.enableAllFirmware = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.tom = {
    name = "tom";
    group = "users";
    extraGroups = [
      "wheel" "disk" "audio" "video" "networkmanager" "systemd-journal"
    ];
    createHome = true;
    uid = 1000;
    home = "/home/tom";
    shell = "/run/current-system/sw/bin/zsh";
  };

  # keep a backup of the configuration
  system.copySystemConfiguration = true;

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "17.09";
}
