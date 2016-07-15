
.PHONY: all

all: base casks

base:
	cat packages.homebrew | xargs brew install
	brew remove macvim # get the macvim command line tools
	brew install macvim --with-override-system-vim

casks:
	cat casks.homebrew | xargs brew cask install

