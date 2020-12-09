{pkgs ? import <nixpkgs>}:
#with import <nixpkgs> {};

pkgs.runCommand "restic_metrics" {
  buildInputs = with pkgs; [ bash restic jq ];
} ''
  mkdir -p $out/bin
  cp ${./restic_metrics.sh} $out/bin/restic_metrics
  #sed -i "2 i export PATH=$PATH" $out/bin/restic_metrics
''

