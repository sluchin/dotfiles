# -*- mode: makefile ; coding: utf-8;
__ALL_SRC__=  .zshrc .zshenv .zlogin
TARGET=$(__ALL_SRC__:%=%.zwc)
INSTALL=install -c

all: check $(TARGET)

check:
	@chmod a-x $(__ALL_SRC__)

%.zwc: %
	zsh -fc "zcompile $*"

install:
	$(INSTALL) $(TARGET) $(HOME)

clean:
	-@rm -f $(TARGET)
