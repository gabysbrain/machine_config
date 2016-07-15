
# Makefile for all the haskell stuff

# TODO: fix this so that hlint and ghc-mod are auto-detected somehow
~/.stack:
	(stack setup || rm -rf $<)
	(stack install hlint ghc-mod || rm -rf $<)

