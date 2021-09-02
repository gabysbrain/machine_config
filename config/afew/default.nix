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

      [InboxFilter]
      tags = -important

      [MailMover]
      folders = vrvis/Inbox vrvis/Sent vrvis/Trash vrvis/Archive vrvis/larvalbrain
      rename = True

      # rules for moving mails
      vrvis/Inbox = 'tag:trash':vrvis/Trash 'tag:sent':vrvis/Sent 'tag:larvalbrain':vrvis/larvalbrain
      vrvis/Sent = 'tag:trash':vrvis/Trash 'tag:inbox':vrvis/Inbox
      vrvis/Trash = 'tag:inbox':vrvis/Inbox 'tag:sent':vrvis/Archive
      vrvis/Archive = 'tag:trash':vrvis/Trash 'tag:inbox':vrvis/Inbox 'tag:sent':vrvis/Archive 'tag:larvalbrain':vrvis/larvalbrain
      vrvis/larvalbrain = 'tag:trash':vrvis/Trash 'tag:inbox':vrvis/Inbox
    '';
  };

  home.file = {
    ".config/afew/student_filter.py".source = ./student_filter.py;
    ".config/afew/SubFolderNameFilter.py".source = ./SubFolderNameFilter.py;
  };
}
