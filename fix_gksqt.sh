#!/bin/sh

GKSQTID=534ab5a2b484a390ae357f2d17ebb0473e160a9b

chmod +w ~/.julia/artifacts/${GKSQTID}/bin/gksqt; nix-shell -p patchelf stdenv --command "patchelf --set-interpreter \"\$(cat \${NIX_CC}/nix-support/dynamic-linker)\" ~/.julia/artifacts/${GKSQTID}/bin/gksqt"

