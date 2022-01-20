{ ... }:

{
  programs = {
    alot = {
      enable = true;
      bindings.global = {
        "%" = "shellescape 'syncmail'; refresh";
        "ctrl f" = "move page down";
        " " = "move page down";
        "ctrl b" = "move page up";
        "q" = "bclose";
        "Q" = "exit";
      };
      bindings.thread = {
        "q" = "bclose";
        "v" = "pipeto urlscan 2>/dev/null";
        # tags
        "a" = "untag inbox; untag todo";
        "d" = "retag trash";
        "j" = "retag spam";
        "u" = "toggletags unread";
      };
      bindings.bufferlist = {
        "q" = "close";
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
        theme = mutt
        tabwidth = 2

        search_threads_sort_order = newest_first
        thread_authors_order_by = latest_message

        edit_headers_whitelist = From, To, Cc, Bcc, Subject
      '';
    };
  };
}

