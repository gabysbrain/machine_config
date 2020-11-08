{pkgs, ...}:
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
        folders = {
          drafts = "drafts";
          sent = "sent";
        };
        passwordCommand = "gpg --quiet --for-your-eyes-only --decrypt ~/.password-store/gmail/personal.gpg | head -1";
        smtp = {
          #host = "smtp.gmail.com";
          tls.enable = true;
        };
        msmtp.enable = true;
        notmuch.enable = true;
      };
      work = {
        realName = "Thomas Torsney-Weir";
        userName = "t.d.torsneyweir@gmail.com";
        address = "t.d.torsneyweir@gmail.com";
        flavor = "gmail.com";
        folders = {
          drafts = "drafts";
          sent = "sent";
        };
        passwordCommand = "gpg --quiet --for-your-eyes-only --decrypt ~/.password-store/gmail/work.gpg | head -1";
        msmtp.enable = true;
        notmuch.enable = true;
      };
    };
  };
  programs = {
    afew = {
      enable = true;
      extraConfig = ''
        [SpamFilter]
        [KillThreadsFilter]
        [ArchiveSentMailsFilter]
        sent_tag = sent

        [StudentFilter]

        [InboxFilter]
        tags = -important
      '';
    };
    msmtp.enable = true; # sendmail support
    notmuch = {
      enable = true; # index email
      new.tags = ["new"];
      new.ignore = [ 
        ".credentials.gmailieer.json"
        ".gmailieer.json"
        ".gmailieer.json.bak"
        ".lock"
        ".state.gmailieer.json"
        ".state.gmailieer.json.bak"
      ];
      search.excludeTags = [ "trash" "spam" ];
    };
  };
  home.packages = with pkgs; [
    neomutt
    (callPackage ../../pkgs/addr_search {})
  ];
  home.file = {
    # neomutt
    ".config/neomutt/neomuttrc".source = ../../../dotfiles/dot-neomuttrc;
    ".config/neomutt/nord.mutt".source = ../../../dotfiles/dot-neomutt/nord.mutt;

    ".mbsyncrc".source = ../../../dotfiles/dot-mbsyncrc;
    ".urlview".source = ../../../dotfiles/dot-urlview;
    ".mailcap".source = ../../../dotfiles/dot-mailcap;
    ".config/alot/themes/tender".source = ../../../dotfiles/dot-alot/tender;
    ".config/afew/student_filter.py".source = ../../../dotfiles/dot-afew/student_filter.py;
  };
}
