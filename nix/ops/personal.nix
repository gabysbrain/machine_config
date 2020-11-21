let 
  diskstationIp = "192.168.0.14";
  rpiIp = "192.168.0.24";
in
{
  network.description = "Home network";

  defaults = {
  };

  media = 
    { config, pkgs, nodes, ... }:
    let
      sambaSecrets = pkgs.writeTextDir "smb-secrets" (builtins.readFile ./secrets/smb-secrets);
    in 
    { deployment.targetHost = rpiIp;
      nixpkgs.system = "aarch64-linux";
      
      imports = [
        ../rpi-configuration.nix
      ];

      fileSystems = {
        "/media/videos" = {
          device = "//${diskstationIp}/videos";
          fsType = "cifs";
          options = let
          # this line prevents hanging on network split
          automount_opts = "x-systemd.automount,noauto,x-systemd.idle-timeout=60,x-systemd.device-timeout=5s,x-systemd.mount-timeout=10s";
          in ["${automount_opts},credentials=${sambaSecrets}/smb-secrets,vers=1.0,file_mode=0660,dir_mode=0770,gid=media,nounix"];
        };
        "/media/music" = {
          device = "//${diskstationIp}/music";
          fsType = "cifs";
          options = let
          # this line prevents hanging on network split
          automount_opts = "x-systemd.automount,noauto,x-systemd.idle-timeout=60,x-systemd.device-timeout=5s,x-systemd.mount-timeout=10s";
          in ["${automount_opts},credentials=${sambaSecrets}/smb-secrets,vers=1.0,file_mode=0660,dir_mode=0770,gid=media,nounix"];
        };
        "/var/lib/jellyfin/metadata" = {
          device = "//${diskstationIp}/jellyfin/metadata";
          fsType = "cifs";
          options = [
            "credentials=${sambaSecrets}/smb-secrets,vers=1.0,file_mode=0660,dir_mode=0770,gid=media,nounix"
          ];
        };
        "/var/lib/jellyfin/transcodes" = {
          device = "//${diskstationIp}/jellyfin/transcodes";
          fsType = "cifs";
          options = [
            "credentials=${sambaSecrets}/smb-secrets,vers=1.0,file_mode=0660,dir_mode=0770,gid=media,nounix"
          ];
        };
      };

      users.groups.media.members = [ "tom" "jellyfish" ];
      services.jellyfin = {
        enable = true;
        group = "media";
      };
      networking.firewall.allowedTCPPorts = [ 80 443 ];

      services.nginx = {
        enable = true;
        recommendedGzipSettings = true;
        recommendedOptimisation = true;
        recommendedProxySettings = true;
        recommendedTlsSettings = true;

        virtualHosts."media" = {
          #addSSL = true;
          #enableACME = true;
          locations."/" = {
            proxyPass = "http://localhost:8096";
          };
        };
      };
    };
}

