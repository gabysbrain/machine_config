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
        # imap through manual offlineimap
        #imap = {
          ##host = "imap.gmail.com";
          #tls.enable = true;
        #};
        smtp = {
          #host = "smtp.gmail.com";
          tls.enable = true;
        };
        passwordCommand = "gpg --quiet --for-your-eyes-only --decrypt ~/.password-store/gmail/personal.gpg | head -1";
        folders = {
          inbox = "INBOX";
          drafts = "[Gmail]/Drafts";
          sent = "[Gmail]/Sent Mail";
          trash = "[Gmail]/Trash";
          #archive = "[Gmail]/All Mail";
        };
        msmtp.enable = true;
        notmuch.enable = true;
        #offlineimap = {
          #enable = true;
        #};
      };
      work = {
        realName = "Thomas Torsney-Weir";
        userName = "t.d.torsney-weir@swansea.ac.uk";
        address = "t.d.torsney-weir@swansea.ac.uk";
        flavor = "plain";
        passwordCommand = "gpg --quiet --for-your-eyes-only --decrypt ~/.password-store/swansea.ac.uk.gpg | head -1";
        # imap through gmail/offlineimap
        #imap = {
          #host = "outlook.office365.com";
          #tls.enable = true;
        #};
        smtp = {
          host = "outlook.office365.com";
          port = 587;
          tls.enable = true;
          tls.useStartTls = true;
        };
        folders = {
          inbox = "INBOX";
          drafts = "[Gmail]/Drafts";
          sent = "[Gmail]/Sent Mail";
          trash = "[Gmail]/Trash";
          #archive = "[Gmail]/All Mail";
        };
        msmtp.enable = true;
        notmuch.enable = true;
        #offlineimap = {
          #enable = true;
        #};
      };
      univie = {
        realName = "Thomas Torsney-Weir";
        userName = "torsnet6";
        address = "thomas.torsney-weir@univie.ac.at";
        flavor = "plain";
        passwordCommand = "gpg --quiet --for-your-eyes-only --decrypt ~/.password-store/univie.ac.at.gpg | head -1";
        # imap through manual offlineimap
        #imap = {
          #host = "imap.univie.ac.at";
          #tls.enable = true;
        #};
        smtp = {
          host = "mail.univie.ac.at";
          tls.enable = true;
        };
        folders = {
          inbox = "INBOX";
          drafts = "INBOX.Drafts";
          sent = "INBOX.Sent";
          trash = "INBOX.Trash";
        };
        msmtp.enable = true;
        notmuch.enable = true;
        #offlineimap = {
          #enable = true;
        #};
      };
    };
  };
}
