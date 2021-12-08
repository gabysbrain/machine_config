{ pkgs, ... }:

let nordColors = {
  nord0 = "rgb(46, 52, 64)"; # background colors
  nord1 = "rgb(59, 66, 82)";
  nord2 = "rgb(67, 76, 94)";
  nord3 = "rgb(76, 86, 106)";
  nord3_bright = "rgb(97, 110, 136)";
  nord4 = "rgb(216, 222, 233)"; # foreground colors
  nord5 = "rgb(229, 233, 240)";
  nord6 = "rgb(236, 239, 244)";
  nord7 = "rgb(143, 188, 187)"; # green
  nord8 = "rgb(136, 192, 208)"; # cyan
  nord9 = "rgb(129, 161, 193)"; # blue
  nord10 = "rgb(94, 129, 172)"; # dark blue
  nord11 = "rgb(191, 97, 106)"; # red
  nord12 = "rgb(208, 135, 112)"; # orange
  nord13 = "rgb(235, 203, 139)"; # yellow
  nord14 = "rgb(163, 190, 140)"; # green
  nord15 = "rgb(180, 142, 173)"; # purple
};
in
{
  programs.broot = {
    enable = true;
    enableZshIntegration = true;
    # based on broot solarized dark and neovim nord themes
    skin = with nordColors; {
      default = "${nord4} ${nord0} / ${nord3} ${nord1}";
      tree = "${nord3} none";
      file = "none none";
      directory = "${nord9} none bold";
      exe = "${nord11} none";
      link = "${nord8} none";
      pruning = "${nord3} none italic";
      perm__ = "${nord3} none";
      perm_r = "none none";
      perm_w = "none none";
      perm_x = "none none";
      owner = "${nord3} none";
      group = "${nord3} none";
      sparse = "none none";
      git_branch = "${nord3} none";
      git_insertions = "${nord14} none";
      git_deletions = "${nord11} none";
      git_status_current = "none none";
      git_status_modified = "${nord13} none";
      git_status_new = "${nord14} none";
      git_status_ignored = "${nord3} none";
      git_status_conflicted = "${nord11} none";
      git_status_other = "${nord11} none";
      selected_line = "none ${nord1}";
      char_match = "${nord14} none underlined";
      file_error = "${nord12} none italic";
      flag_label = "none none";
      flag_value = "${nord13} none bold";
      input = "none none";
      status_error = "${nord12} ${nord3}";
      status_job = "${nord15} ${nord3} bold";
      status_normal = "none ${nord1}";
      status_italic = "${nord13} ${nord3}";
      status_bold = "${nord8} ${nord3} bold";
      status_code = "${nord15} ${nord3}";
      status_ellipsis = "none ${nord3}";
      scrollbar_track = "${nord3} none";
      scrollbar_thumb = "none none";
      help_paragraph = "none none";
      help_bold = "${nord4} none bold";
      help_italic = "${nord4} none italic";
      help_code = "${nord4} ${nord3}";
      help_headers = "${nord13} none";
      help_table_border = "none none";
      preview_title = "gray(20) rgb(0, 43, 54)";
      staging_area_title = "gray(22) rgb(0, 43, 54)";
    };
  };
  home.file = {
    ".config/broot/launcher/refused".source = ./broot-refused;
  };
  programs.zsh  = {
    initExtra = ''
      # ctrl-g to open broot
      # br comes from home-manager broot zsh integration
      bindkey -s '^g' 'br\n'
    '';
  };
}
