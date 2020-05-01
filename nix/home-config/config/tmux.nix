{pkgs, ...}:
{
  programs.tmux = {
    enable = true;
    shortcut = "a";
    terminal = "xterm-256color";
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
