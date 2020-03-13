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
        alot.contactCompletion = {
          type = "shellcommand";
          command = "khard email --parsable";
          regexp = "'^(?P<email>[^@]+@[^\t]+)\t+(?P<name>[^\t]+)'";
          ignorecase = "True";
        };
      };
      work = {
        realName = "Thomas Torsney-Weir";
        userName = "t.d.torsney-weir@swansea.ac.uk";
        address = "t.d.torsney-weir@swansea.ac.uk";
        flavor = "plain";
        folders = {
          drafts = "drafts";
          sent = "sent";
        };
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
          command = "khard email --parsable";
          regexp = "'^(?P<email>[^@]+@[^\t]+)\t+(?P<name>[^\t]+)'";
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
        "ctrl f" = "move page down";
        " " = "move page down";
        "ctrl b" = "move page up";
      };
      bindings.thread = {
        "x" = "bclose";
        "v" = "pipeto urlscan 2>/dev/null";
        # tags
        "a" = "untag inbox; untag todo";
        "d" = "retag trash";
        "j" = "retag spam";
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
        "j" = "retag spam";
        "u" = "toggletags unread";
      };
      extraConfig = ''
        theme = tom.zenburn
        tabwidth = 2
        search_threads_sort_order = newest_first

        reply_force_address = False
      '';
    };
    afew = {
      enable = true;
      extraConfig = ''
        [SpamFilter]
        [KillThreadsFilter]
        [ArchiveSentMailsFilter]

        [Filter.1]
        query = 'asil.cetin@univie.ac.at'
        tags = +students;+students/Asil_Cetin
        message = Asil Cetin

        [Filter.2]
        query = 'd.saunders.910995@swansea.ac.uk'
        tags = +students;+students/David_Saunders
        message = David Saunders

        [Filter.3]
        query = '961297@swansea.ac.uk'
        tags = +students;+students/Etienne_Badoche
        message = Etienne Badoche

        [Filter.4]
        query = 'h.shi.999488@swansea.ac.uk'
        tags = +students;+students/Haiou_Shi
        message = Haiou Shi

        [Filter.5]
        query = 's.james.788390@swansea.ac.uk'
        tags = +students;+students/Sam_James
        message = Sam James

        [InboxFilter]
        tags = -important
      '';
    };
    msmtp.enable = true; # sendmail support
    notmuch = {
      enable = true; # index email
      new.tags = ["new"];
      new.ignore = [ "*.json" ];
      search.excludeTags = [ "trash" "spam" ];
    };
  };
  home.file = {
    ".urlview".source = ../../../dotfiles/dot-urlview;
    ".mailcap".source = ../../../dotfiles/dot-mailcap;
    ".config/alot/themes/tom.zenburn".source = ../../../dotfiles/dot-alot.mytheme;
  };
}
