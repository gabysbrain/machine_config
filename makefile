
IGNORE="install.sh LaunchAgents"

SRCDOTFILES=$(shell ls -d ./dotfiles/* | grep -v -e "zshrc")
DSTDOTFILES=$(SRCDOTFILES:./dotfiles/%=$(HOME)/.%)

.PHONY: all clean

all: $(HOME)/.zshrc $(HOME)/.oh-my-zsh $(DSTDOTFILES) dotfiles/vim/bundle
	make -f brew.mk
	make -f haskell.mk

$(HOME)/.oh-my-zsh:
	sh -c "`curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh`"

$(HOME)/.zshrc: dotfiles/zshrc
	ln $< $@

$(HOME)/.%: dotfiles/%
	ln -F -s $(realpath $<) $@

dotfiles/vim/bundle: dotfiles/vim/install_neobundle.sh
	$<


