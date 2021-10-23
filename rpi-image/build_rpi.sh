#!/bin/sh

if [ "$#" -ne 1 ]; then
  echo "Specify rpi version (3 or 4)"
  exit 1
fi

rpiver=$1

SD=/dev/mmcblk0

SDIMGDIR=`nix-build '<nixpkgs/nixos>' --no-out-link -A config.system.build.sdImage --argstr system aarch64-linux -I nixos-config=./image-${rpiver}.nix`

# if any errors then we should stop
success=$?
if [ "$success" -ne 0 ]; then
  exit $success
fi

SDIMG=`ls ${SDIMGDIR}/sd-image/*.img | head -1`
echo $SDIMG

echo "copying image to ${SD}"
read -p "Press [Enter] to continue ..."

sudo umount ${SD}p*
sudo dd if=${SDIMG} of=${SD} bs=64K status=progress

