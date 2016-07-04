
IGNORE="install.sh LaunchAgents"

SRCDOTFILES=$(shell ls -d ./* | grep -v -e "LaunchAgents" -e "makefile" -e "zshrc")
DSTDOTFILES=$(SRCDOTFILES:./%=$(HOME)/.%)

.PHONY: all clean

all: $(HOME)/.zshrc $(DSTDOTFILES)

$(HOME)/.zshrc: zshrc
	ln $< $@

$(HOME)/.%: %
	ln -F -s $< $@

