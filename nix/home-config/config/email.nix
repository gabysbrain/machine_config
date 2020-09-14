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
        "ctrl f" = "move page down";
        "ctrl b" = "move page up";
        "q" = "bclose";
        "Q" = "exit";
      };
      bindings.thread = {
        "q" = "bclose";
        "v" = "pipeto urlscan 2>/dev/null";
        " " = "fold; untag unread; move next unfolded";
        # tags
        "a" = "untag inbox; untag todo";
        "d" = "retag trash";
        "j" = "retag spam";
        "u" = "toggletags unread";
        "l" = "retagprompt";
      };
      bindings.bufferlist = {
        "q" = "close";
        "enter" = "open";
      };
      bindings.search = {
        "enter" = "select; fold *; unfold tag:unread; move last; unfold";
        # tags
        "a" = "untag inbox; untag todo";
        "d" = "retag trash";
        "j" = "retag spam";
        "u" = "toggletags unread";
      };
      extraConfig = ''
        theme = tender

        tabwidth = 2
        initial_command = "search tag:inbox AND NOT tag:killed; search date:today; search date:yesterday; buffer 0"

        search_threads_sort_order = newest_first
        thread_authors_order_by = latest_message

        [tags]
          [[attachment]]
            translated = 📎
          [[unread]]
            translated = ✉
          [[sent]]
            translated = ✈
          [[replied]]
            translated = ⏎
          [[inbox]]
            translated = 📥
      '';
      hooks = builtins.readFile ../../../dotfiles/dot-alot/hooks.py;
    };
    afew = {
      enable = true;
      extraConfig = ''
        [SpamFilter]
        [KillThreadsFilter]
        [ArchiveSentMailsFilter]

        [StudentFilter]

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
    ".mbsyncrc".source = ../../../dotfiles/dot-mbsyncrc;
    ".urlview".source = ../../../dotfiles/dot-urlview;
    ".mailcap".source = ../../../dotfiles/dot-mailcap;
    ".config/alot/themes/tender".source = ../../../dotfiles/dot-alot/tender;
    ".config/afew/student_filter.py".source = ../../../dotfiles/dot-afew/student_filter.py;
  };
}
