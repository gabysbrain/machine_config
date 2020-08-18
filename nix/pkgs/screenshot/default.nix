{pkgs ? import <nixpkgs>}:
#with import <nixpkgs> {};

pkgs.runCommand "screenshot" {
  buildInputs = with pkgs; [ maim dmenu xdotool ];
} ''
  mkdir -p $out/bin
  cp ${./screenshot} $out/bin/screenshot
  sed -i "2 i export PATH=$PATH" $out/bin/screenshot
''

