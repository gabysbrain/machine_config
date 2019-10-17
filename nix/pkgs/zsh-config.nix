{pkgs ? import <nixpkgs>}:
#with import <nixpkgs> {};

pkgs.writeText "zshrc" ''
export ZSH=${pkgs.oh-my-zsh}/share/oh-my-zsh/

ZSH_THEME="kolo"

alias mm="bundle exec middleman"
alias marked="open -a 'Marked 2'"
alias gvim="vim -g"

# Use the vim editor
export EDITOR=vim

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git vi-mode rbenv osx history npm sbt virutalenv stack)

source $ZSH/oh-my-zsh.sh

# need zmv for moving many things
autoload -U zmv

# function to activate virtualenv
function pyvenv() {
  source ./$1/bin/activate
}

# Code highlighting
function light() {
  if [ -z "$2" ]
    then src="pbpaste"
  else
    src="cat $2"
  fi
  eval ''${src} | highlight -O rtf --syntax $1 --font "Anonymous Pro" --style zenburn --font-size 18 | pbcopy
}

''
