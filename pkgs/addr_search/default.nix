{pkgs ? import <nixpkgs>}:
#with import <nixpkgs> {};

pkgs.writeShellApplication {
  name = "addr_search"; 
  runtimeInputs = with pkgs; [ python311Packages.goobook khard ];
  text = builtins.readFile ./addr_search;
}

