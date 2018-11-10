{ config, pkgs, ... }: 

{
  imports = [
    ../hardware-configuration.nix
    ../networking.nix # generated at runtime by nixos-infect
    ./config/base.nix
  ];

  boot.cleanTmpDir = true;
  networking.hostName = "mrserver";
  networking.firewall = {
    allowPing = true;
    allowedTCPPorts = [ 80 443 8384 ];
  };
  services = {
    openssh.enable = true;
    nginx = {
      enable = true;
      virtualHosts = {
        "www.tomtorsneyweir.com" = {
          #serverName = "website";
          default = false;
          enableSSL = false;
          forceSSL = false;
          locations = {
            "/" = {
              root = "/home/tom/site/";
              index = "index.html";
              extraConfig = "autoindex on;";
            };
          };
        };
      };
    };
  };

  system.autoUpgrade.enable = true;

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

  # root only has ssh login
  /*users.users.root.openssh.authorizedKeys.keys = [*/
    /*"ssh-rsa AAAAB3NzaC1yc2EAAAABIwAAAIEAwDPUjo80GFY2FO9bDH9cAXo7n7SiUKjXIHzfRMfsAqD9Rk/puV+W4QRvT0XOZSEZQf3gifcPM/raA35BVmAzAa2jYISWeUWIqYf+AcipFrMKKqS639Q9/GgJL2STr6Gh0EVHsGcFJpuJ8GO5eqnKR0ZYl3j9bpMO/WpgkAw7hUU= tom@katana"*/
  /*];*/
  users.users.tom.openssh.authorizedKeys.keys = [
    "ssh-rsa AAAAB3NzaC1yc2EAAAABIwAAAIEAwDPUjo80GFY2FO9bDH9cAXo7n7SiUKjXIHzfRMfsAqD9Rk/puV+W4QRvT0XOZSEZQf3gifcPM/raA35BVmAzAa2jYISWeUWIqYf+AcipFrMKKqS639Q9/GgJL2STr6Gh0EVHsGcFJpuJ8GO5eqnKR0ZYl3j9bpMO/WpgkAw7hUU= tom@katana"
  ];
}

