#{ config, pkgs, requireFile, ... }:

final: prev: {
  openrgb = prev.openrgb.overrideAttrs ( old: {
    src = prev.fetchFromGitLab {
      owner = "CalcProgrammer1";
      repo = "OpenRGB";
      rev = "783259a870f2c754afa3f8de54d91d2d959009e2";
      #sha256 = "0000000000000000000000000000000000000000000000000000";
      sha256 = "0cvwydp3chih79k3kyz5snvcfh1p72lqizzfa81x6wdmgsxx7fqh";
    };

    buildInputs = old.buildInputs ++ [ prev.mbedtls ];
  });
}

