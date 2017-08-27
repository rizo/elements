
build:
	jbuilder build -j4 @install

install: build
	jbuilder install

uninstall:
	jbuilder uninstall

reinstall: uninstall install

test:
	jbuilder runtest

clean:
	jbuilder clean

utop: build utop.ml
	/usr/bin/env bash -c 'utop -init <(cat ~/.ocamlinit utop.ml)'

watch:
	ls src/*.ml* tests/*.ml* | entr -cr make test

.PHONY: build install uninstall reinstall test clean utop watch

