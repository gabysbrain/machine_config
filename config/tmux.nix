{pkgs, ...}:
{
  programs.tmux = {
    enable = true;
    shortcut = "a";
    terminal = "screen-256color";
    extraConfig = ''
      # reload config
      unbind r
      bind r source-file ~/.config/tmux/tmux.conf

      # windows start at 1
      set -g base-index 1

      # Vi copypaste mode
      set-window-option -g mode-keys vi
      bind-key -T copy-mode-vi 'v' send -X begin-selection
      bind-key -T copy-mode-vi 'y' send -X copy-selection-and-cancel

      # get true color working
      set-option -ga terminal-overrides ',*-256color*:Tc'

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

      # new windows should open in current dir
      bind '"' split-window -c "#{pane_current_path}"
      bind % split-window -h -c "#{pane_current_path}"
      bind c new-window -c "#{pane_current_path}"

      # Turn on mouse mode
      set -g mouse on

      # status bar styling
      set-option -g status-position bottom
      set -g status-justify left
      set -g status-style 'bg=#323232 fg=#eeeeee dim'
      set -g status-left ' '
      set -g status-right '#{?session_group,#{session_group},#{session_name}}'
      set -g status-right-length 50
      set -g status-left-length 20

      # window styling
      setw -g window-status-current-style 'fg=#282828 bg=#bbbbbb bold'
      setw -g window-status-current-format ' #[fg=#464632]#I#[fg=#282828]:#W #[fg=#c9d05c]#F '
      #setw -g window-status-current-format ' #[fg=#9faa00]#I#[fg=#282828]:#W #[fg=#c9d05c]#F '

      setw -g window-status-style 'fg=#eeeeee bg=#444444'
      setw -g window-status-format ' #[fg=#b3deef]#I#[fg=#eeeeee]:#W #[fg=#44778d]#F '

      setw -g window-status-bell-style 'fg=#282828 bg=#f43753 bold'

      # fix vim slow escape key thing
      set -sg escape-time 0
    '';
  };
  programs.zsh.shellAliases = {
    # tmux stuff
    ta = "tmux attach -t";
    tad = "tmux attach -d -t";
    ts = "tmux new-session -s";
    tl = "tmux list-sessions";
    tkss = "tmux kill-session -t";
    tt = "tmux new-session -t";
  };
  home.packages = [
    (pkgs.callPackage ../pkgs/tat {})
  ];
}
