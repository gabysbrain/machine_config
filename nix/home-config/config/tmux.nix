{pkgs, ...}:
{
  programs.tmux = {
    enable = true;
    shortcut = "a";
    terminal = "tmux-256color";
    plugins = with pkgs; [
      {
        plugin = tmuxPlugins.resurrect;
        extraConfig = "set -g @resurrect-strategy-vim 'session'";
      }
      {
        plugin = tmuxPlugins.continuum;
        extraConfig = ''
          set -g @continuum-restore 'on'
          set -g @continuum-save-interval '60' # minutes
        '';
      }
    ];
    extraConfig = ''
      # reload config
      unbind r
      bind r source-file ~/.tmux.conf

      # split panes using | and -
      bind | split-window -h
      bind - split-window -v
      unbind '"'
      unbind %

      # Fast pane switching
      bind -n M-Left select-pane -L
      bind -n M-Right select-pane -R
      bind -n M-Up select-pane -U
      bind -n M-Down select-pane -D

      #urxvt tab like window switching (-n: no prior escape seq)
      bind -n S-down new-window
      bind -n S-left prev
      bind -n S-right next
      bind -n C-left swap-window -t -1
      bind -n C-right swap-window -t +1

      # Turn on mouse mode
      set -g mouse on

      # status bar styling
      set-option -g status-position top

      # fix vim slow escape key thing
      set -sg escape-time 0

      # Zenburn colors
      setw -g clock-mode-colour colour117
      setw -g mode-style 'fg=colour117 bg=colour238 bold'
      set -g status-style 'fg=colour248 bg=colour235'
      setw -g window-status-current-style 'fg=colour223 bg=colour237 bold'
      set -g message-style 'fg=colour117 bg=colour235 bold'
      set -g status-left '#[fg=colour187,bold]'
    '';
  };
  programs.zsh.shellAliases = {
    # tmux stuff
    ta = "tmux attach -t";
    tad = "tmux attach -d -t";
    ts = "tmux new-session -s";
    tl = "tmux list-sessions";
    tkss = "tmux kill-session -t";
  };
  home.packages = [
    (pkgs.callPackage ../../pkgs/tat {})
  ];
}
