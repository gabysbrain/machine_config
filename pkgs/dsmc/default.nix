{config, lib, pkgs, stdenv, ...}:
#with import <nixpkgs> {};

let 
srcDeb = pkgs.fetchurl {
  url = http://aix.software.ibm.com/storage/tivoli-storage-management/maintenance/client/v7r1/Linux/LinuxX86_DEB/BA/v718/7.1.8.0-TIV-TSMBAC-LinuxX86_DEB.tar;
  sha256 = "1wszaa82vv01xfi7jkpz5izywnsvyikx45phrk7ywksk7c5qdl03";
};
gskit = stdenv.mkDerivation rec {
  name = "gskit-${version}";
  version = "8.0-50.78";

  src = srcDeb;

  dontBuild = true;
  #dontPatchELF = true;
  dontStrip = true;
  nativeBuildInputs = [ pkgs.dpkg pkgs.makeWrapper ];

  unpackCmd = ''
    mkdir root
    tar xf $curSrc
    dpkg -x gskcrypt64_${version}.linux.x86_64.deb root
    dpkg -x gskssl64_${version}.linux.x86_64.deb root
  '';

  installPhase = ''
    mkdir -p $out/lib
    cp -r usr/local/ibm/gsk8_64/lib64/* $out/lib/
  '';
};
in stdenv.mkDerivation rec {
  name = "dsmc-${version}";
  version = "7.1.8.0";

  src = srcDeb;
  libPath = lib.makeLibraryPath [ gskit stdenv.cc.cc ];

  dontBuild = true;
  #dontPatchELF = true;
  dontStrip = true;

  buildInputs = [ pkgs.dpkg pkgs.makeWrapper pkgs.jre ];
  #nativeBuildInputs = [ pkgs.dpkg pkgs.makeWrapper ];

  sysCfg = pkgs.writeText "dsm.sys" ''
    SERVERNAME ${config.serverName}
    NODENAME ${config.nodename}
    TCPSERVERADDRESS ${config.serverAddress}
    ${lib.concatMapStringsSep "\n" (x: "EXCLUDE " + x) config.excludes}
    ${lib.concatMapStringsSep "\n" (x: "EXCLUDE.DIR " + x) config.excludeDirs}
  '';
  optCfg = pkgs.writeText "dsm.opt" ''
    SERVERNAME ${config.serverName}
    PASSWORD ${config.password}
  '';
    #${lib.optionalString (config.password != null) ("PASSWORD " + config.password)}
  #sysCfg = ./dsm.sys;
  #optCfg = ./dsm.opt;

  unpackCmd = ''
    mkdir root
    tar xf $curSrc
    dpkg -x tivsm-api64.amd64.deb root
    dpkg -x tivsm-ba.amd64.deb root
  '';

  installPhase = ''
    mkdir -p $out/bin $out/lib
    cp -r opt $out/

    for lib in libgpfs.so libdmapi.so libApiTSM64.so libxmlutil-7.1.8.0.so; do
      patchelf --set-rpath ${libPath}:$out/lib \
        $out/opt/tivoli/tsm/client/api/bin64/$lib
      ln -s $out/opt/tivoli/tsm/client/api/bin64/$lib $out/lib/$lib
    done
    ln -s $out/opt/tivoli/tsm/client/api/bin64/libtsmxerces-c.so.28.0 $out/lib/libtsmxerces-c.so.28
    ln -s $out/opt/tivoli/tsm/client/api/bin64/libtsmxerces-depdom.so.28.0 $out/lib/libtsmxerces-depdom.so.28

    for exec in dsmc dsmadmc dsmagent dsmcad dsmcert dsmswitch dsmtrace; do
      patchelf --set-interpreter $(cat ${stdenv.cc}/nix-support/dynamic-linker) \
               --set-rpath ${libPath}:$out/lib \
        $out/opt/tivoli/tsm/client/ba/bin/$exec
      wrapProgram $out/opt/tivoli/tsm/client/ba/bin/$exec \
        --set DSM_DIR $out/opt/tivoli/tsm/client/ba/bin \
        --set DSM_LOG /var/log
      ln -s $out/opt/tivoli/tsm/client/ba/bin/$exec $out/bin/$exec
    done


    cp ${sysCfg} $out/opt/tivoli/tsm/client/ba/bin/dsm.sys
    cp ${optCfg} $out/opt/tivoli/tsm/client/ba/bin/dsm.opt
  '';
    #ln -s $out/opt/tivoli/tsm/client/ba/bin/dsmj $out/bin/dsmj
}

