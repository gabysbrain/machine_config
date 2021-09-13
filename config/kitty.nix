{pkgs, ...}:

let 
  kitty-nord-theme = pkgs.stdenv.mkDerivation rec {
    pname = "nord-kitty";
    version = "3a819c1f207cd2f98a6b7c7f9ebf1c60da91c9e9";
    name = "${pname}-${version}";

    src = pkgs.fetchFromGitHub {
      owner = "connorholyday";
      repo = "nord-kitty";
      rev = "${version}";
      sha256 = "0j3byv5wfd7kiq6k8l1g38zg68p19vgbd1hfj6llnfqhdfkspfb5";
    };

    unpackPhase = false;
    installPhase = ''
      mkdir -p $out/share/config/kitty
      cp $src/nord.conf $out/share/config/kitty/
    '';
  };
in
{
  programs.kitty = {
    enable = true;
    font = {
      package = pkgs.nerdfonts;
      name = "Anonymice Nerd Font";
      size = 11;
    };
    settings = {
      scrollback_lines = 10000;
      enable_audio_bell = false;
      update_check_interval = 0;
      cursor_stop_blinking_after = 0;
    };

    extraConfig = ''
      include ${kitty-nord-theme}/share/config/kitty/nord.conf
    '';
  };
}
