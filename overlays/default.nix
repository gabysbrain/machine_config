final: prev: 

rec {
  slock = prev.slock.overrideAttrs ( old: {
    src = prev.fetchFromGitHub {
      owner = "gabysbrain";
      repo = "slock";
      rev = "37f091cb167f719103ef70baa6b46b95645e5b95";
      #sha256 = "0000000000000000000000000000000000000000000000000000";
      sha256 = "0q2l3y6pm817y0gib3z0bq0h3c95lcys0wz9vmlb779q76spkr7v";
    };
  });

  #openrgb = (import ../overlays/openrgb.nix final prev);

  python = prev.python.override {
    self = prev.python;
    packageOverrides = import ./pyjira.nix;
  };
  python3 = prev.python3.override {
    self = prev.python3;
    packageOverrides = import ./pyjira.nix;
  };

}
