{pkgs ? import <nixpkgs>}:
#with import <nixpkgs> {};

/*
pkgs.runCommand "restic-metrics" {
  buildInputs = with pkgs; [ bash hostname restic jq ];
} ''
  mkdir -p $out/bin
  cp ${./restic-metrics.sh} $out/bin/restic-metrics
  #sed -i "2 i export PATH=$PATH" $out/bin/restic-metrics
''
*/

pkgs.runCommandLocal "restic-metrics" {
  script = ./restic-metrics.sh;
  nativeBuildInputs = [ pkgs.makeWrapper ];
} ''
  makeWrapper $script $out/bin/restic-metrics \
    --prefix PATH : ${pkgs.lib.makeBinPath [ pkgs.bash pkgs.jq pkgs.hostname pkgs.restic ]}
''

