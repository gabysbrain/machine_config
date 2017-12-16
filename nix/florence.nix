with import <nixpkgs> {};

stdenv.mkDerivation rec {
  name = "florence-${version}";
  version = "0.6.3";
  src = fetchurl {
    url = "mirror://sourceforge/project/florence/florence/0.6.3/florence-0.6.3.tar.bz2";
    sha256 = "07h9qm22krlwayhzvc391lr23vicw81s48g7rirvx1fj0zyr4aa2";
  };
  isExecutable = true;

  preConfigure = ''
    configureFlagsArray+=("--without-xrecord" "--without-at-spi" "--without-docs")
  '';

  nativeBuildInputs = [wrapGAppsHook];
  buildInputs = [ pkgconfig intltool gnome2.scrollkeeper gnome3.gtk libxml2 librsvg gst_all_1.gstreamer libnotify xorg.libXtst ];

  meta = {
    homepage = http://florence.sourceforge.net;
    platforms = stdenv.lib.platforms.linux;
    license = stdenv.lib.licenses.gpl2;
  };
}
