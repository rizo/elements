
build:
	jbuilder build -j4 @install
	@# Only install the top-level `Proto` module.
	@# See: https://github.com/janestreet/jbuilder/pull/106
	@sed -i '' '/Proto_/d' _build/default/proto.install

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
