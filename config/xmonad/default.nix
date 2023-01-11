{ pkgs, config, nixosConfig, ... }:

let
  syncthing-status = pkgs.runCommand "syncthing-status" {
    buildInputs = with pkgs; [
      (callPackage ../../pkgs/syncthing-quick-status.nix {})
    ];
  } ''
    mkdir -p $out/bin
    cp ${./syncthing-status.sh} $out/bin/syncthing-status
    sed -i "2 i export PATH=$PATH" $out/bin/syncthing-status
  '';
in
{
  imports = [
    ./polybar.nix
  ];
  xsession = {
    enable = true;

    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      config = ./xmonad.hs;
      extraPackages = haskellPackages: [
        haskellPackages.dbus
      ];
    };
  };
  home.file = {
    ###############################
    # XMonad utilities
    ###############################
    # TODO: make this configure based on machine capabilities
    #".xmonad/xmobar.hs".source = ./. + "/xmobar-${nixosConfig.networking.hostName}.hs";
    #".xmonad/net.sh" = {
      #source = ./net.sh;
      #executable = true;
    #};
  };
  home.packages = with pkgs; [
    xmonad-log
  ];
}
