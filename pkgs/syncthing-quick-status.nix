{pkgs ? import <nixpkgs>}:
#with import <nixpkgs> {};

pkgs.stdenv.mkDerivation rec {
  version = "9076da1c2f0bfc85bc77dc7d2adb688c32303a8e";
  name = "syncthing-quick-status-${version}";

  src = pkgs.fetchFromGitHub {
    owner = "serl";
    repo = "syncthing-quick-status";
    rev = "${version}";
    sha256 = "sha256-4aVXcM2wGeWgJpz48MrFLgpK2aKplaCEPIyn7EQ0OL0=";
  };

  buildInputs = [
    pkgs.jq pkgs.curl
  ];

  patchPhase = ''
    patchShebangs .
  '';

  installPhase = ''
    mkdir -p $out/bin
    cp syncthing-quick-status.sh $out/bin/syncthing-quick-status
  '';
}

