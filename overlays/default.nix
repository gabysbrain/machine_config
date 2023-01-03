final: prev: 

rec {
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
