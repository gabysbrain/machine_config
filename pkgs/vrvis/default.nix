{ pkgs, stdenv }:
#with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "vrvis-utils";
  src = ./.;

  # TODO: add vpn service config here

  buildPhase = false;

  installPhase = ''
    mkdir -p $out/bin
    cp $src/vrvis_connect $out/bin/
    cp $src/vrvis_disconnect $out/bin/

    # fix sshfs path
    sed -i 's#^sshfs#${pkgs.sshfs}/bin/sshfs#' $out/bin/vrvis_connect
    sed -i 's#^sshfs#${pkgs.sshfs}/bin/sshfs#' $out/bin/vrvis_disconnect
  '';
}
