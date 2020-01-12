PROGS=hw

.PHONY: all
all: $(patsubst %,%.exe,$(PROGS))
	if ! readlink ~/.ocamlinit ; then ln -s `pwd`/.ocamlinit ~/ ; fi

%.exe : %.ml
	if test -L $@; then unlink $@; fi
	dune build $@
	ln -s _build/default/$@ $(patsubst %.exe,%,$@)

.PHONY: clean
clean:
	rm -rvf _build $(PROGS)
