#!/bin/sh

rangex=( 0 9640 )
rangey=( 0 7220 )
#torotate=( 10 11 )
torotate=( 10 )

xrandrout="$(xrandr)"

echo $xrandrout
case $xrandrout in
  *0+0\ inverted*) rotate=0; swap=0; invert=(0 0);; # go back to normal
  *0+0\ left*)     rotate=2; swap=0; invert=(1 1);; # go to inverted
  *0+0\ \(normal*) rotate=1; swap=1; invert=(1 0);; # go to reading mode
esac

echo ${invert[0]}
xrandr -o $(( rotate ))
for input in ${torotate[@]}; do
 xinput set-prop $input "Evdev Axes Swap" $swap
 xinput set-prop $input "Evdev Axis Inversion" ${invert[0]}, ${invert[1]}
 #xinput set-prop $input "Evdev Axis Calibration" ${cal[@]}
done

