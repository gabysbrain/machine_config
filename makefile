
IGNORE="install.sh LaunchAgents"

SRCDOTFILES=$(shell ls -d ./dotfiles/* | grep -v -e "zshrc")
DSTDOTFILES=$(SRCDOTFILES:./dotfiles/%=$(HOME)/.%)

.PHONY: all clean

all: $(HOME)/.zshrc $(HOME)/.oh-my-zsh $(DSTDOTFILES)
	make -f haskell.mk

$(HOME)/.oh-my-zsh:
	git submodule init
	git submodule update
	ln -s -F $(realpath oh-my-zsh) $@

$(HOME)/.zshrc: dotfiles/zshrc
	ln $< $@

$(HOME)/.%: dotfiles/%
	ln -F -s $(realpath $<) $@

vim/bundle: vim/install_neobundle.sh
	$<


