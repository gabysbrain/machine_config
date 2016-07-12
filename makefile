
IGNORE="install.sh LaunchAgents"

SRCDOTFILES=$(shell ls -d ./* | grep -v -e "LaunchAgents" -e "makefile" -e "zshrc")
DSTDOTFILES=$(SRCDOTFILES:./%=$(HOME)/.%)

.PHONY: all clean

all: $(HOME)/.zshrc $(HOME)/.oh-my-zsh $(DSTDOTFILES)

$(HOME)/.oh-my-zsh:
	git submodule init
	git submodule update
	ln -s -F $(realpath oh-my-zsh) $@

$(HOME)/.zshrc: zshrc
	ln $< $@

$(HOME)/.%: %
	ln -F -s $< $@

