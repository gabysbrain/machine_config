{ config, pkgs, ... }:

# packages needed for the modules that I'm teaching
{
  services.mysql = {
    enable = true;
    dataDir = "/var/db/mysql";
    package = pkgs.mysql;
  };
}
