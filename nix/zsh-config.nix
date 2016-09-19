with import <nixpkgs> {};

pkgs.writeText "zshrc" ''
export ZSH=${pkgs.oh-my-zsh}/share/oh-my-zsh/

if [[ "$OSTYPE" == "darwin"* ]]; then
    ZSH_THEME="theunraveler"
else
    ZSH_THEME="gentoo"
fi

plugins=(git)

if [[ "$OSTYPE" == "darwin"* ]]; then
    # Nix
    export NIX_PATH=nixpkgs=/Users/marc/nixpkgs
    source ~/.nix-profile/etc/profile.d/*.sh
fi

source $ZSH/oh-my-zsh.sh

export EDITOR=vim
bindkey -v
bindkey '^R' history-incremental-search-backward

alias gitc="git checkout"
alias gits="git status"
''
