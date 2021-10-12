{ pkgs, ... }:

# PIM utilities (calendar, email, tasks, etc)
{
  imports = [
    ./email.nix
    ./taskwarrior/default.nix
    ./vdirsyncer/default.nix
    ./khal/default.nix
    ./khard/default.nix
  ];

  home.packages = with pkgs; [
  ];
}
