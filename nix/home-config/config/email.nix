{pkgs, ...}:
let 
  gmail = {name, email, primary ? false}: {
    primary = primary;
    realName = "Thomas Torsney-Weir";
    userName = email;
    address = email;
    flavor = "gmail.com";
    folders = {
      drafts = "drafts";
      sent = "sent";
    };
    passwordCommand = "gpg --quiet --for-your-eyes-only --decrypt ~/.password-store/gmail/${name}.gpg | head -1";
    msmtp.enable = true;
    notmuch.enable = true;
    lieer.enable = true;
  };
in 
{
  accounts.email = {
    maildirBasePath = ".mail";
    accounts = {
      personal = gmail {name="personal"; email="torsneyt@gmail.com"; primary=true;};
      work = gmail {name="work"; email="t.d.torsneyweir@gmail.com";};
      sfu = gmail {name="sfu"; email="ttorsney.sfu@gmail.com";};
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
