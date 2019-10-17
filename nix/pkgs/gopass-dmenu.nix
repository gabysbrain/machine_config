{pkgs ? import <nixpkgs>}:

pkgs.writeShellScriptBin "gopass-dmenu" ''
    GOPASS=${pkgs.gopass}/bin/gopass
    MENU=${pkgs.dmenu}/bin/dmenu
    XDO=${pkgs.xdotool}/bin/xdotool

    $GOPASS ls --flat | $MENU | xargs --no-run-if-empty $GOPASS show -f | head -n 1 | $XDO type --clearmodifiers --file -
''

