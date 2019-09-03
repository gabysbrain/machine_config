#{pkgs ? import <nixpkgs>}:
with import <nixpkgs> {};

pkgs.writeShellScriptBin "screenselect" ''
    MAIN="eDP-1"
    # important that screens go left to right
    # first screen in list will become primary
    DOCK="DP-2 DP-1-1"
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
      "docking station") 
        # turn off the laptop screen temporalily to reset the display numbers
        $XRANDR --output $MAIN --off 
        rs=$MAIN
        fst=true
        for s in $DOCK; do
          if [ "$fst" = true ]; then
            fst=false
            $XRANDR --output $s --mode '1920x1080' --primary
          else
            $XRANDR --output $s --mode '1920x1080' --right-of $rs
          fi
          rs=$s
          done
        $XRANDR --output $MAIN --mode '1920x1080' --right-of $rs
        ;;
      dual) 
        $XRANDR --output $MAIN --auto $(echo "$screens" | grep -v $MAIN | awk '{print "--output", $1, "--same-as $MAIN"}' | tr '\n' ' ') 
        ;;
      main) 
        $XRANDR --output $MAIN --auto $(echo "$screens" | grep -v $MAIN | awk '{print "--output", $1, "--off"}' | tr '\n' ' ') 
        ;;
      other) 
        $ARANDR
        exit 
        ;;
    esac
''

