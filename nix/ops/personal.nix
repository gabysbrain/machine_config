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
      wasabiSecrets = pkgs.writeTextDir "wasabi" (builtins.readFile ./secrets/wasabi);
      resticSecrets = pkgs.writeTextDir "restic-password" (builtins.readFile ./secrets/restic-password);
    in 
    { deployment.targetHost = rpiIp;
      nixpkgs.system = "aarch64-linux";

      networking.hostName = "media";
      
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
          in ["credentials=${sambaSecrets}/smb-secrets,vers=1.0,file_mode=0660,dir_mode=0770,gid=media,nounix"];
        };
        "/media/music" = {
          device = "//${diskstationIp}/music";
          fsType = "cifs";
          options = let
          # this line prevents hanging on network split
          automount_opts = "x-systemd.automount,noauto,x-systemd.idle-timeout=60,x-systemd.device-timeout=5s,x-systemd.mount-timeout=10s";
          in ["credentials=${sambaSecrets}/smb-secrets,vers=1.0,file_mode=0660,dir_mode=0770,gid=media,nounix"];
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

      #security.acme.email = "torsneyt@gmail.com";
      #security.acme.acceptTerms = true;

      services.nginx = {
        enable = true;
        recommendedGzipSettings = true;
        recommendedOptimisation = true;
        recommendedProxySettings = true;
        recommendedTlsSettings = true;

        virtualHosts."media.tomtorsneyweir.com" = {
          #addSSL = true;
          #enableACME = true;
          locations."/" = {
            proxyPass = "http://localhost:8096";
          };
        };
      };

      # backup media server
      services.restic.backups = {
        remote = {
          paths = [ 
            "/media" 
            "/var/lib/jellyfin"
          ];
          repository = "s3:https://s3.eu-central-1.wasabisys.com/gabysbrain-restic";
          passwordFile = "${resticSecrets}/restic-password";
          s3CredentialsFile = "${wasabiSecrets}/wasabi";
          extraBackupArgs = [
            "--host=media"
          ];
          timerConfig = {
            OnBootSec = "2m";
            OnUnitInactiveSec = "1d";
          };
        };
      };
    };
}

