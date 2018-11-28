#export OCAMLFIND_IGNORE_DUPS_IN=/home/walter/.opam/4.04.0/lib/ocaml/compiler-libs

OCAMLBUILD = ocamlbuild -use-ocamlfind

PKGS=cairo2,cairo2.lablgtk2,lablgtk2,lablgtk2-extras,extlib
#,ocamlgraph,ocamlgraph.dgraph

dbg:
	$(OCAMLBUILD) -cflags -annot,-bin-annot -pkg $(PKGS) -I src -I qt src/main.d.byte

opt:
	$(OCAMLBUILD) -ocamlc ocamlopt -ocamlopt ocamlopt -cflags -unsafe,-annot,-bin-annot,-noassert,-unbox-closures,-unboxed-types,-nodynlink,-O3,-rounds=4,-unbox-closures-factor=100 -pkg $(PKGS) -I src src/main.native

prof:
	$(OCAMLBUILD) -cflags -annot,-bin-annot -pkg $(PKGS) -I src src/main.p.native

perf:
	sudo perf record -g --call-graph=dwarf ./main.native 0

report:
	sudo perf report -G --tui

debugger: dbg
	oqamldebug ./main.d.byte

test: all
	$(OCAMLBUILD) -pkg $(PKGS),oUnit -Is src,test test/test.native
	./test.native

clean:
	$(OCAMLBUILD) -clean
