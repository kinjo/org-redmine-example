EMACS=emacs

EMACSC=$(EMACS) -batch -f batch-byte-compile

all: el-expectations.elc el-mock.elc

el-expectations.elc: el-expectations.el
	$(EMACSC) $^

el-mock.elc: el-mock.el
	$(EMACSC) $^

test: el-expectations.elc el-mock.elc
	./test-runner.sh test-*.el

clean:
	rm -f *.elc TAGS

tags: *.el
	ctags -e *.el
