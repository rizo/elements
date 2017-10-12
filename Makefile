
build:
	jbuilder build -j4 @install
	@# Only install the top-level `Proto` module.
	@# See: https://github.com/janestreet/jbuilder/pull/106
	@sed '/Proto__/d' _build/default/proto.install > /tmp/proto_install \
	    && mv /tmp/proto_install _build/default/proto.install

install: build
	jbuilder install

uninstall:
	jbuilder uninstall

reinstall: uninstall install

test:
	jbuilder runtest

clean:
	jbuilder clean

live-test:
	which entr
	ls */*.ml* | entr -r make test

.PHONY: build install uninstall reinstall test clean live-test
