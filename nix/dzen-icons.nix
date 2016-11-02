with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "dzen-icons-1.0.0";
  src = fetchgit {
    url = https://github.com/kniren/dotfiles.git;
    sha256 = "13xg7c3x7wxhyq7h87p2grc713mz1v8vnwv9gs6nr1k6w22lmm66";
  };
  /*
  src = fetchurl {
    url = ftp://ftp.nluug.nl/pub/gnu/hello/hello-2.1.1.tar.gz;
  };
  */

  meta = {
    description = "Icons for dzen2";
  };

  configurePhase = "";
  buildPhase = "";
  installPhase = ''
    cp -R $src/xmonad/dzen/icons/ $out
  '';
}
