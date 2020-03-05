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
        alot.contactCompletion = {
          type = "shellcommand";
          command = "khard email -s";
          regexp = "'^(?P[^@]+@[^\\t]+)\\t+(?P[^\\t]+)'";
          ignorecase = "True";
        };
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
        alot.contactCompletion = {
          type = "shellcommand";
          command = "khard email -s";
          regexp = "'^(?P[^@]+@[^\\t]+)\\t+(?P[^\\t]+)'";
          ignorecase = "True";
        };
      };
    };
  };
  programs = {
    alot = {
      enable = true;
      bindings.global = {
        "%" = "shellescape 'syncmail'; refresh";
        "x" = "bclose";
      };
      bindings.thread = {
        "x" = "bclose";
        "v" = "pipeto urlscan 2>/dev/null";
        # tags
        "a" = "untag inbox; untag todo";
        "d" = "retag trash";
        "s" = "retag spam";
        "u" = "toggletags unread";
      };
      bindings.bufferlist = {
        "x" = "close";
        "enter" = "open";
      };
      bindings.search = {
        "enter" = "untag unread; select";
        # tags
        "a" = "untag inbox; untag todo";
        "d" = "retag trash";
        "s" = "retag spam";
        "u" = "toggletags unread";
      };
      extraConfig = ''
        theme = mutt
      '';
    };
    afew = {
      enable = true;
    };
    msmtp.enable = true; # sendmail support
    notmuch = {
      enable = true; # index email
      new.tags = ["new"];
      search.excludeTags = [ "trash" "spam" ];
    };
  };
  home.file = {
    ".urlview".source = ../../../dotfiles/dot-urlview;
    ".mailcap".source = ../../../dotfiles/dot-mailcap;
  };
}
