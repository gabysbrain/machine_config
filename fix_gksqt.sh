#!/bin/sh

#GKSQTID=534ab5a2b484a390ae357f2d17ebb0473e160a9b
#GKSQTID=26099abd23e11fc440259049997307380be745c8
GKSQTID=ba11969bdd27bbb55be64bfadc0de51a013e945a

chmod +w ~/.julia/artifacts/${GKSQTID}/bin/gksqt; nix-shell -p patchelf stdenv --command "patchelf --set-interpreter \"\$(cat \${NIX_CC}/nix-support/dynamic-linker)\" ~/.julia/artifacts/${GKSQTID}/bin/gksqt"

