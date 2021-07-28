{ pkgs, ... }:
{
  programs.afew = {
    enable = true;
    extraConfig = ''
      [SpamFilter]
      [KillThreadsFilter]
      [ArchiveSentMailsFilter]
      sent_tag = sent

      [StudentFilter]
      list_file = /home/tom/Sync/studentlist.csv

      [InboxFilter]
      tags = -important
    '';
  };

  home.file = {
    ".config/afew/student_filter.py".source = ./student_filter.py;
  };
}
