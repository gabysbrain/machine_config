{ config, pkgs, ... }:

{

  environment.systemPackages = with pkgs; [ openrgb ];

  boot.kernelModules = [ "i2c-dev" "i2c-piix4" ];
  
  #services.udev.extraRules = "${pkgs.openrgb}/etc/udev/rules.d/60-openrgb.rules";
  services.udev.packages = [ pkgs.openrgb ];
  #services.udev.extraRules =  builtins.readFile openrgb-rules;
}
