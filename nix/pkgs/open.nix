{pkgs ? import <nixpkgs>}:
#with import <nixpkgs> {};

pkgs.writeShellScriptBin "open" ''
  fname=$1
  OPENER=${pkgs.mimeo}/bin/mimeo
  case $(${pkgs.file}/bin/file --mime-type $fname -b) in
    text/*) $EDITOR $fname;;
    image/*) ${pkgs.sxiv}/bin/sxiv -a $fname ;;
    application/pdf) ${pkgs.zathura}/bin/zathura $fname;;
    *) $OPENER $fname ;;
  esac
''

