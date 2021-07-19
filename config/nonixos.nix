{ pkgs, ... }:

{
  programs.zsh.profileExtra = ''
    emulate sh
    . ~/.profile
    emulate zsh
  '';
}
