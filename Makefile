

LISPSRCS:=$(shell find . -name "*.lisp")
ASDSRCS:=$(wildcard *.asd)

LISP=sbcl
CLFLAGS=--non-interactive --no-userinit

all: booker.image

booker.image: $(LISPSRCS) $(ASDSRCS) version
	$(LISP) $(CLFLAGS) --load build.lisp
clean:
	-rm -f **/*.fasl
	-rm -f booker.image
