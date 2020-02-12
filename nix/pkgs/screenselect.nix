{pkgs ? import <nixpkgs>}:
#with import <nixpkgs> {};

pkgs.writeShellScriptBin "screenselect" ''
    MAIN="eDP1"
    # important that screens go left to right
    # first screen in list will become primary
    #DOCK="DP2 DP1-1"
    DOCK="DP2 HDMI1"
    # default keyboard
    DEFAULTKB=gb
    XRANDR=${pkgs.xorg.xrandr}/bin/xrandr
    ARANDR=${pkgs.arandr}/bin/arandr
    KBSWITCH=${pkgs.xkb-switch}/bin/xkb-switch

    # Gives a number of options for connecting to different screens

    # Get all possible displays
    screens=$($XRANDR -q | grep "connected")

    # Get all connected screens.
    connscreens=$(echo "$screens" | grep " connected" | awk '{print $1}')

    kbmap=$DEFAULTKB

    # Let the user choose what screen
    chosen=$(printf "docking station\\ndual\\nmain\\nother" | dmenu -i -p "Select display:") &&
    case "$chosen" in
      "docking station") 
        # use us keyboard for docking station
        kbmap=us
        # turn off the laptop screen temporarily to reset the display numbers
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

    # set kb map
    $KBSWITCH -s $kbmap
''

