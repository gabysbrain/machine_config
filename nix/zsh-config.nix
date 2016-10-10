with import <nixpkgs> {};

pkgs.writeText "zshrc" ''
export ZSH=${pkgs.oh-my-zsh}/share/oh-my-zsh/

ZSH_THEME="kolo"

alias mm="bundle exec middleman"
alias marked="open -a 'Marked 2'"

# Use the vim editor
export EDITOR=vim

# Java's home
export JAVA_HOME=`/usr/libexec/java_home`

# R's home
export R_HOME=/Library/Frameworks/R.framework/Resources

# Where jrebel lives
export JREBEL_PATH=/Applications/jrebel/jrebel.jar

# Customize to your needs...
#export PATH=~/.playground/bin:~/.local/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/opt/X11/bin:/usr/X11/bin:/opt/local/bin:/Library/TeX/texbin/:~/Projects/dotfiles/bin/

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git vi-mode rbenv osx history npm sbt brew virutalenv stack)

source $ZSH/oh-my-zsh.sh

# need zmv for moving many things
autoload -U zmv

# customize prompt

# nvm through homebrew is special
export NVM_DIR=~/.nvm
source $(brew --prefix nvm)/nvm.sh
#PROMPT="''${user} %{$fg[blue]%}%4(c:...:)%2c%{$reset_color%}$ "

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
