{ config, pkgs, ... }: 

{
  imports = [
    ../../hardware-configuration.nix
    ../../networking.nix # generated at runtime by nixos-infect
    ./config/base.nix
  ];

  boot.cleanTmpDir = true;
  boot.kernel.sysctl = { # need more watch nodes
    # Note that inotify watches consume 1kB on 64-bit machines.
    "fs.inotify.max_user_watches"   = 1048576;   # default:  8192
    "fs.inotify.max_user_instances" =    1024;   # default:   128
    "fs.inotify.max_queued_events"  =   32768;   # default: 16384
    # fix out of memory errors
    "vm.overcommit_memory" = 1;
  };
  networking.hostName = "mrserver";
  networking.firewall = {
    allowPing = true;
    allowedTCPPorts = [ 80 443 8384 ];
  };
  services = {
    openssh = {
      permitRootLogin = "yes";
      enable = true;
    };
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

  # automatically update nixos
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
  home-manager.users.tom = import ./home-config/server.nix; # needs to be a function


  users.users.tom.openssh.authorizedKeys.keys = [
    "ssh-rsa AAAAB3NzaC1yc2EAAAABIwAAAIEAwDPUjo80GFY2FO9bDH9cAXo7n7SiUKjXIHzfRMfsAqD9Rk/puV+W4QRvT0XOZSEZQf3gifcPM/raA35BVmAzAa2jYISWeUWIqYf+AcipFrMKKqS639Q9/GgJL2STr6Gh0EVHsGcFJpuJ8GO5eqnKR0ZYl3j9bpMO/WpgkAw7hUU= tom@katana"
  ];

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "18.09";
}

