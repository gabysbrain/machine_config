#!/bin/sh

net_up () {
  ip link show | grep "$1" | grep 'state UP' > /dev/null
}

if net_up wlp; then
  WLAN=直
else
  WLAN="<fc=#dddddd>睊</fc>"
fi

if (net_up enp) || (net_up eth); then
  ETH=
else
  ETH="<fc=#dddddd></fc>"
fi

echo "${WLAN} ${ETH}"

