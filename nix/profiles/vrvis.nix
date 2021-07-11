{ pkgs, ... }:

{
  home.packages = with pkgs; [
    remmina # rdp
    zulip   # chat
  ];
}
