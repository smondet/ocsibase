.PHONY: mount_ocsibase build all install static
all: build



_build/ocsibase:
	mkdir -p _build/ocsibase
_build/ocsibase/%.eliom: src/webapp/%.eliom
	CC=$$PWD ; cd _build/ocsibase ; ln -s ../../$< ; cd $$CC 
_build/ocsibase/Makefile: src/webapp/Makefile
	CC=$$PWD ; cd _build/ocsibase ; ln -s ../../$< ; cd $$CC 

TO_MOUNT=$(patsubst src/webapp/%,_build/ocsibase/%,${wildcard src/webapp/*})
mount_ocsibase:: _build/ocsibase $(TO_MOUNT)

build: mount_ocsibase
	ocaml setup.ml -build simple_pam.mllib
	make -C _build/ocsibase all
	ocaml setup.ml -build

static:
	ocamlfind ocamlopt -linkall \
	    -package ocsigenserver,ocsigenserver.ext.ocsipersist-sqlite  \
	    -package eliom.server,ocsigenserver.ext.staticmod,core  \
             -I _build/src/simple_pam/ simple_pam.cmxa \
	     _build/ocsibase/ocsibase.cmxa \
	     server_main.cmx -o ocsibaseserver -linkpkg -thread

install: build
	ocaml setup.ml -reinstall

uninstall:
	ocaml setup.ml -uninstall

doc:
	ocaml setup.ml -doc

clean:
	ocaml setup.ml -clean


# clean everything and uninstall
fresh: clean uninstall

.PHONY: doc install uninstall clean fresh
