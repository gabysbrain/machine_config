{pkgs ? import <nixpkgs>}:
#with import <nixpkgs> {};

pkgs.writeShellScriptBin "screenselect" ''
    MAIN="eDP-1"

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

        $XRANDR --output HDMI-1 --mode '1920x1080' --primary
        #$XRANDR --output DP-2 --mode '1920x1080' --rate 50.0 --left-of HDMI-1
        $XRANDR --output eDP-1 --mode '1920x1080' --right-of HDMI-1
        ;;
      dual) 
        $XRANDR --output $MAIN --auto $(echo "$connscreens" | grep -v $MAIN | awk -v main=$MAIN '{print "--output", $1, "--same-as", main}' | tr '\n' ' ') 
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

