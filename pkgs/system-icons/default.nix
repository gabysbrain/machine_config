{pkgs ? import <nixpkgs>, stdenv }:
#with import <nixpkgs> {};

# TODO: pick up the list of icons automatically
let systems = [ "philadelphia" "brokkoli" "katana" ];
in stdenv.mkDerivation {
  name = "system-icons";
  description = "icons for lightdm for each GUI system";

  src = ./.;

  dontUnpack = true;

  # TODO: ensure icons are 64x64 for lightdm
  installPhase = ''
    mkdir -p $out/share/icons/64x64/

    ln -s $src/philadelphia.png $out/share/icons/64x64/
    ln -s $src/brokkoli.png $out/share/icons/64x64/
    ln -s $src/katana.png $out/share/icons/64x64/
  '';
} 

