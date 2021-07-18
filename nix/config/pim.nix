{ pkgs, ... }:

# PIM utilities (calendar, email, tasks, etc)
{
  imports = [
    ../config/email.nix
    ../config/taskwarrior/default.nix
    ../config/vdirsyncer/default.nix
    ../config/khal/default.nix
    ../config/khard/default.nix
  ];

  home.packages = with pkgs; [
    (callPackage ../pkgs/syncmail {})
    gmailieer
  ];
}
