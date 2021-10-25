#{ config, pkgs, requireFile, ... }:

final: prev: {
  imagej = prev.imagej.overrideAttrs ( old: {
    nativeBuildInputs = old.nativeBuildInputs ++ [ prev.glib prev.wrapGAppsHook ];
  });
}

