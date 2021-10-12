{pkgs, ...}:
{
  home.packages = with pkgs; [
    neomutt
    urlscan
    w3m
  ];
  home.file = {
    # neomutt
    ".config/neomutt/neomuttrc".source = ./dot-neomuttrc;
    ".config/neomutt/nord.mutt".source = ./dot-neomutt/nord.mutt;

    ".urlview".source = ./dot-urlview;
    ".mailcap".source = ./dot-mailcap;
  };
}
