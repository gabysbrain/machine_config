# Edit this configuration file to define what should be installed on
# your system. Help is available in the configuration.nix(5) man page, on
# https://search.nixos.org/options and in the NixOS manual (`nixos-help`).

# NixOS-WSL specific options are documented on the NixOS-WSL repository:
# https://github.com/nix-community/NixOS-WSL

{ config, lib, pkgs, ... }:

let proxyServer = "http://proxy-flex.p1at.s-group.cc:8080";
in
{
  imports = [
    # NixOS-WSL module is done in flake
    ./nixos/common.nix
    ./nixos/laptop.nix
    ./nixos/desktop.nix
  ];

  wsl.enable = true;
  wsl.defaultUser = "soy9a";

  networking.proxy.httpProxy = proxyServer;
  networking.proxy.httpsProxy = proxyServer;
  
  users.users.soy9a = {
    home = "/home/soy9a";
    description = "Thomas Torsney-Weir";
    extraGroups = [ "wheel" "lp" "lpadmin" "adbusers" "dialout" ]; # Enable ‘sudo’ for the user.
    createHome = true;
    shell = "/run/current-system/sw/bin/zsh";
    isNormalUser = true;
  };

  # docker
  virtualisation.docker.enable = true;
  users.extraGroups.docker.members = [ "soy9a" ];

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It's perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.05"; # Did you read the comment?
}
