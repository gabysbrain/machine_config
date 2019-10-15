{pkgs ? import <nixpkgs>}:

# include patch from https://github.com/NixOS/nixpkgs/pull/56520/commits/5ad7553a527ad69c7f2d5642463f5d336a18beb4 to correctly handle google calendar
pkgs.vdirsyncer.overrideAttrs (oldAttrs: rec {
  patches = [(pkgs.fetchpatch {
      url = https://github.com/pimutils/vdirsyncer/pull/788.patch;
      sha256 = "0vl942ii5iad47y63v0ngmhfp37n30nxyk4j7h64b95fk38vfwx9";
    })];
})
