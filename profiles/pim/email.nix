{pkgs, config, ...}:
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
    alot.contactCompletion = {
      type = "shellcommand";
      command = "khard email --parsable";
      regexp = "'^(?P<email>[^@]+@[^\t]+)\t+(?P<name>[^\t]+)'";
      ignorecase = "True";
    };
  };
in 
{
  imports = [
    ./alot.nix
  ];
  accounts.email = {
    maildirBasePath = ".mail";
    accounts = {
      personal = gmail {name="personal"; email="torsneyt@gmail.com"; primary=true;};
      swansea = gmail {name="swansea"; email="t.d.torsneyweir@gmail.com";};
      sfu = gmail {name="sfu"; email="ttorsney.sfu@gmail.com";};
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
    (callPackage ../../pkgs/addr_search {})
    (callPackage ../../pkgs/syncmail {})
    (callPackage ../../pkgs/notmuch-tag {})
    (callPackage ../../pkgs/notmuch-imap-tag-mover {})

    notmuch
    lieer
  ];
}
