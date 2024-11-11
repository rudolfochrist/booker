# Makefile  -*- mode: makefile-gmake; -*-

include config.mk

IMAGE_NAME = $(PACKAGE_NAME)

LISPSRCS = $(shell find . -name "*.lisp")
ASDSRCS = $(wildcard *.asd)

all: $(IMAGE_NAME).image

$(IMAGE_NAME).image: $(ASDSRCS) $(LISPSRCS) version config.lisp
	-rm -f $(IMAGE_NAME).image
	$(CL) $(CLFLAGS) \
	--load config.lisp \
	--eval '(asdf:load-system "$(PACKAGE_NAME)" :force t)' \
	--eval '(gc :full t)' \
	--eval '(asdf:make "$(PACKAGE_NAME)/image")'

check:
	$(CL) $(CLFLAGS) \
	--load config.lisp \
	--eval '(asdf:test-system "$(PACKAGE_NAME)" :force t)'
clean:
	-find . -name "*.fasl" -delete
	-rm -f $(PACKAGE_NAME).info
	-rm -f $(IMAGE_NAME).image

distclean: clean
	-rm -f config.mk config.status config.lisp

info: $(PACKAGE_NAME).info

$(PACKAGE_NAME).info: documentation/$(PACKAGE_NAME).texi documentation/dict.texi
	$(MAKEINFO) $(srcdir)/$<

documentation/dict.texi: $(ASDSRCS) $(LISPSRCS) version config.lisp
	$(CL) $(CLFLAGS) \
	--load config.lisp \
	--eval '(asdf:make "$(PACKAGE_NAME)/documentation")'
