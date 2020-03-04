{
  accounts.email = {
    maildirBasePath = ".mail";
    accounts = {
      personal = {
        primary = true;
        realName = "Tom Torsney-Weir";
        userName = "torsneyt@gmail.com";
        address = "torsneyt@gmail.com";
        flavor = "gmail.com";
        smtp = {
          #host = "smtp.gmail.com";
          tls.enable = true;
        };
        passwordCommand = "gpg --quiet --for-your-eyes-only --decrypt ~/.password-store/gmail/personal.gpg | head -1";
        msmtp.enable = true;
        notmuch.enable = true;
      };
      work = {
        realName = "Thomas Torsney-Weir";
        userName = "t.d.torsney-weir@swansea.ac.uk";
        address = "t.d.torsney-weir@swansea.ac.uk";
        flavor = "plain";
        passwordCommand = "gpg --quiet --for-your-eyes-only --decrypt ~/.password-store/swansea.ac.uk.gpg | head -1";
        smtp = {
          host = "outlook.office365.com";
          port = 587;
          tls.enable = true;
          tls.useStartTls = true;
        };
        msmtp.enable = true;
        notmuch.enable = true;
      };
    };
  };
  programs = {
    alot = {
      enable = true;
      extraConfig = ''
        theme = mutt
      '';
    };
    afew = {
      enable = true;
    };
    #offlineimap.enable = true; # email syncing
    msmtp.enable = true; # sendmail support
    notmuch = {
      enable = true; # index email
      new.tags = ["new"];
      search.excludeTags = [ "trash" "spam" ];
    };
  };
  home.file = {
    #".muttrc".source = ../../dotfiles/dot-muttrc;
    #".mutt".source = ../../dotfiles/dot-mutt;
    ".urlview".source = ../../../dotfiles/dot-urlview;
    #".offlineimaprc".source = ../../dotfiles/dot-offlineimap;
    #".offlineimap.py".source = ../../dotfiles/dot-offlineimap.py;
  };
}
