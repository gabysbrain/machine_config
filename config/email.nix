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
  imports = [
    ../config/afew/default.nix
  ];
  accounts.email = {
    maildirBasePath = ".mail";
    accounts = {
      personal = gmail {name="personal"; email="torsneyt@gmail.com"; primary=true;};
      swansea = gmail {name="swansea"; email="t.d.torsneyweir@gmail.com";};
      sfu = gmail {name="sfu"; email="ttorsney.sfu@gmail.com";};
      vrvis = {
        realName = "Thomas Torsney-Weir";
        userName = "torsney-weir";
        address = "torsney-weir@vrvis.at";
        flavor = "plain";
        folders = {
          drafts = "Drafts";
          sent = "Sent";
          trash = "Trash";
        };
        imap.host = "mail.vrvis.at";
        smtp.host = "mail.vrvis.at";
        passwordCommand = "cat /run/secrets/vrvis";
        msmtp.enable = true;
        notmuch.enable = true;
        mbsync = {
          enable = true;
          create = "both";
          expunge = "both";
        };
      };
    };
  };
  programs = {
    mbsync.enable = true;
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
  /*
  services = {
    mbsync = {
      enable = true;
      frequency = "*:0/15";
      postExec = "${pkgs.notmuch}/bin/notmuch new";
    };
  };
  */
  home.packages = with pkgs; [
    neomutt
    (callPackage ../pkgs/addr_search {})
    (callPackage ../pkgs/syncmail {})
    (callPackage ../pkgs/notmuch-imap-tag-mover {})

    alot
    notmuch
    gmailieer
    urlscan
    w3m
  ];
  home.file = {
    # neomutt
    ".config/neomutt/neomuttrc".source = ./neomutt/dot-neomuttrc;
    ".config/neomutt/nord.mutt".source = ./neomutt/dot-neomutt/nord.mutt;

    ".urlview".source = ./neomutt/dot-urlview;
    ".mailcap".source = ./neomutt/dot-mailcap;
    ".config/alot/themes/tender".source = ./alot/tender;
  };
}
