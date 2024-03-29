{ pkgs, ... }:

{
  programs = {
    alot = {
      enable = true;
      bindings.global = {
        "%" = "shellescape 'syncmail'; refresh";
        "ctrl f" = "move page down";
        "ctrl b" = "move page up";
        "q" = "bclose";
        "Q" = "exit";
        "/" = "prompt 'search '";
      };
      bindings.thread = {
        "q" = "bclose";
        "v" = "pipeto urlscan 2>/dev/null";
        " " = "fold; untag unread; move next unfolded";
        # tags
        "l" = "retagprompt";
        "a" = "untag inbox; untag todo";
        "d" = "retag trash";
        #"s" = "retag spam";
        "u" = "toggletags unread";
      };
      bindings.bufferlist = {
        "q" = "close";
        "enter" = "open";
      };
      bindings.search = {
        "enter" = "untag unread; select";
        # tags
        "l" = "retagprompt";
        "a" = "untag inbox; untag todo";
        "d" = "retag trash";
        "s" = "retag spam";
        "u" = "toggletags unread";
      };
      settings = {
        theme = "mutt";
        tabwidth = 2;
        initial_command = "search tag:inbox AND NOT tag:killed; search date:today; search date:yesterday; buffer 0";
        search_threads_sort_order = "newest_first";
        thread_authors_order_by = "latest_message";
        edit_headers_whitelist = "From, To, Cc, Bcc, Subject";
        ask_subject = false;
        auto_remove_unread = true;
      };
    };
  };
  home.packages = with pkgs; [
    w3m
    urlscan
  ];
  home.file = {
    ".mailcap".source = ./dot-mailcap;
  };
}

