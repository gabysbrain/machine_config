#{pkgs ? import <nixpkgs>}:
with import <nixpkgs> {};

pkgs.writeShellScriptBin "screenselect" ''
    MAIN="eDP-1"
    DOCK="DP-1"
    XRANDR=${pkgs.xorg.xrandr}/bin/xrandr
    ARANDR=${pkgs.arandr}/bin/arandr

    # Gives a number of options for connecting to different screens

    # Get all possible displays
    screens=$($XRANDR -q | grep "connected")

    # Get all connected screens.
    connscreens=$(echo "$screens" | grep " connected" | awk '{print $1}')

    # Let the user choose what screen
    chosen=$(printf "docking station\\ndual\\nmain\\nother" | dmenu -i -p "Select display:") &&
    case "$chosen" in
      "docking station") $XRANDR --output $DOCK --off; $XRANDR --output $DOCK --mode '1920x1080' --left-of $MAIN ;;
      dual) $XRANDR --output $MAIN --auto $(echo "$screens" | grep -v $MAIN | awk '{print "--output", $1, "--same-as $MAIN"}' | tr '\n' ' ') ;;
      main) $XRANDR --output $MAIN --auto $(echo "$screens" | grep -v $MAIN | awk '{print "--output", $1, "--off"}' | tr '\n' ' ') ;;
      other) $ARANDR ; exit ;;
    esac
''

