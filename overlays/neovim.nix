#{ config, pkgs, requireFile, ... }:

let unstable = import <unstable> {};
in
final: prev: {
  neovim = unstable.neovim;
  neovim-unwrapped = unstable.neovim-unwrapped;
}

