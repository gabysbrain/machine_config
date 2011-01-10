# Set vi keys
set -o vi

# Set the prompt so its pretty
PS1="[\u@\h \w][\!]>"

# Setup my path
export PATH=$PATH:/usr/local/sbin
export PATH=$PATH:/sbin
export PATH=$PATH:/usr/sbin
export PATH=$PATH:/usr/local/bin
export PATH=$PATH:/bin
export PATH=$PATH:/usr/bin
export PATH=$PATH:/usr/share/java/apache-ant/bin
export PATH=$PATH:.

# Set the PAGER
export PAGER=less

# Set the EDITOR
export EDITOR=vi

# Aliases
alias gvim='gvim -X'
alias vi='vim'
alias .='pwd'
alias ..='cd ..'
alias cd..='cd ..'
alias cdwd='cd `pwd`'
alias cwd='echo $cwd'
alias files='find \!:1 -type f -print'  # files x => list files in x
alias ff='find . -name \!:1 -print'  # ff x => find file named x
alias line='sed -n '\''\!:1 p'\'' \!:2'    # line 5 file => show line 5 of file
alias l='ls -l'
alias ll='ls -la \!* | $PAGER'
alias ls='ls -F'
alias m='$PAGER'
alias h='history'
alias pu='pushd'
alias po='popd'

