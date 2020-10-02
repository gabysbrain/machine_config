{pkgs ? import <nixpkgs>}:
#with import <nixpkgs> {};

pkgs.runCommand "addr_search" {
  buildInputs = with pkgs; [ 
    (pkgs.callPackage ../goobook.nix {})
    khard
  ];
} ''
  mkdir -p $out/bin
  cp ${./addr_search} $out/bin/addr_search
  sed -i "2 i export PATH=$PATH" $out/bin/addr_search
''

