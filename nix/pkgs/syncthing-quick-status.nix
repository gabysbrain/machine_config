{pkgs ? import <nixpkgs>}:
#with import <nixpkgs> {};

pkgs.stdenv.mkDerivation rec {
  version = "68a6a5427c83fb2b45646cab19a641e598b1b38e";
  name = "syncthing-quick-status-${version}";

  src = pkgs.fetchFromGitHub {
    owner = "serl";
    repo = "syncthing-quick-status";
    rev = "${version}";
    sha256 = "031lax6j1m0139vn1dx52f8l69f64i8ypkxx6y9kgh4a08lbnqj3";
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

