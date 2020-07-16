#!/bin/sh

net_up () {
  ip link show | grep $1 | grep 'state UP' > /dev/null
}

if net_up wlan0; then
  WLAN=
else
  WLAN="<fc=#eeeeee></fc>"
fi

if net_up eth0; then
  ETH=
else
  ETH="<fc=#eeeeee></fc>"
fi

echo "${WLAN} ${ETH}"

