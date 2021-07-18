#! /usr/bin/env nix-shell
#! nix-shell -i sh -p cabal2nix

mydir=`dirname $0`

cabal2nix /home/tom/Projects/zk/ > ${mydir}/default.nix

