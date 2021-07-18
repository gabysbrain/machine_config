{ pkgs, ... }:

{
  home.file = {
    ".julia/config/startup.jl".source = ./startup.jl;
  };
}

