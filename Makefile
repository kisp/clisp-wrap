prefix=/usr/local

install:
	install -m 0755 dist/build/clisp-wrap/clisp-wrap $(prefix)/bin
