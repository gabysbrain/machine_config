{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    # haskell
    cabal-install
    ghc
    stack
    hlint
    #haskellPackages.ghc-mod
    haskellPackages.hdevtools
    #haskellPackages.hfmt

    # node 
    nodejs
    nodePackages.node2nix

    # general
    gnumake
    jq
    entr

    chromium
  ];

  nix.binaryCaches = [ "https://cache.nixos.org/" "https://nixcache.reflex-frp.org" ];
  nix.binaryCachePublicKeys = [ "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=" ];
}
