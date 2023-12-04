{ config, pkgs, ... }:

{
  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  # firewall config
  networking.firewall.enable = true;

  environment.systemPackages = with pkgs; [
    nox
    git
    vim
    atool
    file


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
		fontDir.enable = true;
		enableGhostscriptFonts = true;
		packages = with pkgs; [
		  corefonts  # Micrsoft free fonts
		  unifont # some international languages
      powerline-fonts
      anonymousPro
		];
  };

  nixpkgs.config.allowUnfree = true;

  # set build cores automatically
  nix.settings.cores = 0;

  # List services that you want to enable

  # periodically run GC
  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 10d";
  };
  nix.settings.auto-optimise-store = true;

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
}
