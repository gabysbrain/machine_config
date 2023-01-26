{ pkgs, lib, ...}:
{
  programs.fzf = {
    colors = {
      # nord colors from https://github.com/ianchesal/nord-fzf
      fg = "e5e9f0";
      bg = "#3b4252";
      hl = "#81a1c1";
      "fg+" = "#e5e9f0";
      "bg+" = "#3b4252";
      "hl+" = "#81a1c1";
      info = "#eacb8a";
      prompt = "#bf6069";
      pointer = "#b48dac";
      marker = "#a3be8b";
      spinner = "#b48dac";
      header = "#a3be8b";
    };
  };
}
