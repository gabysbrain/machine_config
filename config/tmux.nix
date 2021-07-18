{pkgs, ...}:
{
  programs.tmux = {
    enable = true;
    shortcut = "a";
    terminal = "screen-256color";
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

      # get true color working
      set-option -ga terminal-overrides ',xterm-termite:RGB'
      #set-option -ga terminal-overrides ',*-256color*:Tc'

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
      set -g status-justify left
      set -g status-style 'bg=#323232 fg=#eeeeee dim'
      set -g status-left ' '
      set -g status-right ' '
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
  };
  home.packages = [
    (pkgs.callPackage ../pkgs/tat {})
  ];
}
