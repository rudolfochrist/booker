# sb-texinfo

.POSIX:
.SUFFIXES:
.SUFFIXES:  .asd .lisp .txt

VERSION=$(shell cat version)
SB_TEXINFO=sb-texinfo
PACKAGE=sb-texinfo-$(VERSION)

# variables
DOTEMACS=$(HOME)/.emacs.d/init.el

ASDSRCS=$(wildcard *.asd)
LISPSRCS=$(wildcard *.lisp)
SRCS=$(ASDSRCS) $(LISPSRCS)


# paths
scrdir=.

prefix=/usr/local
exec_prefix=$(prefix)
bindir=$(exec_prefix)/bin
libdir=$(exec_prefix)/lib
libexecdir=$(exec_prefix)/libexec/$(SB_TEXINFO)
lispdir=$(exec_prefix)/lisp/$(SB_TEXINFO)

datarootdir=$(prefix)/share
datadir=$(datarootdir)/$(SB_TEXINFO)
docdir=$(datarootdir)/doc/$(SB_TEXINFO)
infodir=$(datarootdir)/info

# programs
INSTALL=/usr/bin/install
LS=/usr/local/bin/gls
MAKEINFO=/usr/local/bin/makeinfo

all:

clean:
	-rm **/*.fasl

distclean: clean
	-rm -rf $(PACKAGE)
	-rm $(PACKAGE).tar.gz

dist: distclean
	mkdir -p $(PACKAGE)
	cp -R $(shell $(LS) --ignore $(PACKAGE)) $(PACKAGE)
	tar czf $(PACKAGE).tar.gz $(PACKAGE)
	-rm -rf $(PACKAGE)

install: installdirs
	cp -R $(shell $(LS)) $(DESTDIR)$(lispdir)

uninstall:
	-rm -rf $(lispdir)

installdirs:
	mkdir -p $(DESTDIR)$(lispdir)

info: sb-texinfo.info

sb-texinfo.info: sb-texinfo.texi
	$(MAKEINFO) $(srcdir)/sb-texinfo.info

check:

web:
	rm -rf web
	mkdir web
	make -C doc html pdf
	cp doc/*.html doc/*.pdf web/
	cp web/sb-texinfo.html web/index.html

pages: web
	git checkout gh-pages
	cp web/* .
	git commit -a -c master
	rm -rf web
	git checkout -f master

.PHONY: all clean distclean dist install installdirs uninstall info check web
