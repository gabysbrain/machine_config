{ pkgs, ... }:
{
  programs.afew = {
    enable = true;
    extraConfig = ''
      [SpamFilter]
      [KillThreadsFilter]
      [ArchiveSentMailsFilter]
      sent_tag = sent

      [SubFolderNameFilter]
      subfolder = vrvis
      folder_blacklist = Archive
      folder_lowercases = true

      [StudentFilter]
      list_file = /home/tom/Sync/studentlist.csv

      [InboxFilter]
      tags = -important

      [MailMover]
      folders = vrvis/Inbox vrvis/Sent vrvis/Trash vrvis/Archive
      rename = True

      # rules for moving mails
      vrvis/Inbox = 'tag:trash':vrvis/Trash 'tag:sent':vrvis/Sent 
      vrvis/Sent = 'tag:trash':vrvis/Trash 'tag:inbox':vrvis/Inbox
      vrvis/Trash = 'tag:inbox':vrvis/Inbox 'tag:sent':vrvis/Archive
      vrvis/Archive = 'tag:trash':vrvis/Trash 'tag:inbox':vrvis/Inbox 'tag:sent':vrvis/Archive
    '';
  };

  home.file = {
    ".config/afew/student_filter.py".source = ./student_filter.py;
    ".config/afew/SubFolderNameFilter.py".source = ./SubFolderNameFilter.py;
  };
}
