#{ config, pkgs, requireFile, ... }:

let unstable = import <unstable> {};
in
final: prev: {
  zotero = unstable.zotero;
}

