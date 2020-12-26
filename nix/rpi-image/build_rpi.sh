#!/bin/sh

SD=/dev/mmcblk0

BUILDLOG=`nixos-generate -f sd-aarch64 --system aarch64-linux -c image.nix`
IMGDIR=$(dirname $(echo $BUILDLOG | tail -1))/..

SDIMG=$(ls $IMGDIR/sd-image/*.img)

echo $SDIMG

echo "copying image to ${SD}"
read -p "Press [Enter] to continue ..."

sudo umount ${SD}p*
sudo dd if=${SDIMG} of=${SD} bs=64K status=progress

