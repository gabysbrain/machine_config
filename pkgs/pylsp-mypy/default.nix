{ system ? builtins.currentSystem, pkgs ? import <nixpkgs> { inherit system; } }:
#with import <nixpkgs> {};

let
  mach-nix = import (pkgs.fetchFromGitHub {
    owner = "DavHau";
    repo = "mach-nix";
    # place version number with the latest one from the github releases page
    rev = "3.5.0";
    sha256 = "sha256-j/XrVVistvM+Ua+0tNFvO5z83isL+LBgmBi9XppxuKA=";
  }) { 
    #python = "python310"; 
    inherit system;
  };

in
  # need the system variable for nix flakes
mach-nix.lib."${system}".buildPythonPackage rec {
  # contents of a requirements.txt (use builtins.readFile ./requirements.txt alternatively)
  #requirements = builtins.readFile "${src}/requirements.txt";
  src = pkgs.fetchFromGitHub {
    owner  = "python-lsp";
    repo   = "pylsp-mypy";
    rev    = "0.6.4"; # versions after this require python-lsp-server > 1.7 
                      # which nixos doesn't have now
    sha256 = "sha256-BpYg2noReHFgJ/5iQI09XUWNAN7UdcYgqpZ/IPr17Ao=";
  };
  providers = {
    tomli = "nixpkgs";
    typing-extensions = "nixpkgs";
  };
}

