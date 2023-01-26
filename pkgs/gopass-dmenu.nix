{pkgs ? import <nixpkgs>}:

pkgs.writeShellApplication {
  name = "gopass-dmenu";
  runtimeInputs = with pkgs; [ gopass dmenu xdotool ];
  text = ''
    MENU=dmenu

    gopass ls --flat | $MENU | xargs --no-run-if-empty gopass show -o -f | head -n 1 | xdotool type --clearmodifiers --file -
  '';
}

