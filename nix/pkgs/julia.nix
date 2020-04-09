{pkgs, stdenv, ...}:

let
  ldconfigWrapper = stdenv.mkDerivation {
    name = "ldconfig-env";

    nativeBuildInputs = [ pkgs.makeWrapper ];

    phases = [ "installPhase" "fixupPhase" ];

    installPhase = ''
      makeWrapper ${pkgs.glibc.bin}/bin/ldconfig $out/sbin/ldconfig \
        --add-flags "-C /usr/ld.so.cache"
    '';
  };
  extraLibs = pkgs: with pkgs; [
    ldconfigWrapper

    git
    gitRepo
    gnupg
    autoconf
    curl
    procps
    gnumake
    utillinux
    m4
    gperf
    unzip
    libGLU_combined
    xorg.libXi xorg.libXmu freeglut
    xorg.libXext xorg.libX11 xorg.libXv xorg.libXrandr zlib
    ncurses5
    stdenv.cc
    binutils

    # Nvidia note: may need to change cudnn to match cudatoolkit version
    #cudatoolkit_10_0
    #cudnn_cudatoolkit_10_0
    #linuxPackages.nvidia_x11

    julia_11
    #vim
    #atom

    # Arpack.jl
    arpack
    gfortran.cc
    (pkgs.runCommand "openblas64_" {} ''
      mkdir -p "$out"/lib/
      ln -s ${openblasCompat}/lib/libopenblas.so "$out"/lib/libopenblas64_.so.0
    '')

    # IJulia.jl
    #mbedtls
    #zeromq3
    #python3Packages.jupyterlab
    # ImageMagick.jl
    imagemagickBig
    # HDF5.jl
    hdf5
    # Cairo.jl
    cairo
    gettext
    pango.out
    glib.out
    # Gtk.jl
    gtk3
    gdk_pixbuf
    # GZip.jl # Required by DataFrames.jl
    gzip
    zlib
    # GR.jl # Runs even without Xrender and Xext, but cannot save files, so those are required
    xorg.libXt
    xorg.libX11
    xorg.libXrender
    xorg.libXext
    glfw
    freetype
    qt4
  ];
in
  pkgs.buildFHSUserEnv {
    name = "julia"; # so that we can start julia with 'julia'
    #version = julia.version;
    multiPkgs = pkgs: [ pkgs.zlib ];
    targetPkgs = extraLibs;
    profile = with pkgs; ''
      export LD_LIBRARY_PATH=${lib.makeLibraryPath (extraLibs pkgs)}
      export EXTRA_CCFLAGS="-I/usr/include"
    '';
    extraBuildCommands = ''
      # Cannot write to /etc?
      echo "$out/lib" > $out/usr/ld.so.conf
      ldconfig -f $out/usr/ld.so.conf -C $out/usr/ld.so.cache
    '';
    runScript = "julia";
  }
