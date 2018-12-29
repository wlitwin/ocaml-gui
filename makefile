#export OCAMLFIND_IGNORE_DUPS_IN=/home/walter/.opam/4.04.0/lib/ocaml/compiler-libs

OCAMLBUILD = ocamlbuild -use-ocamlfind

PKGS=cairo2,cairo2.lablgtk2,lablgtk2,lablgtk2-extras,base,extlib,core
#,ocamlgraph,ocamlgraph.dgraph
JS_PKGS=js_of_ocaml,js_of_ocaml-ocamlbuild,js_of_ocaml-ppx

dbg:
	$(OCAMLBUILD) -cflags -annot,-bin-annot,'-open Base' -pkg $(PKGS) -tag thread -I src -I gui src/main.d.byte

js:
	$(OCAMLBUILD) -plugin-tag "package(js_of_ocaml.ocamlbuild)" -tag thread -cflags -annot,-bin-annot,'-open Base' -pkg $(PKGS) -pkg $(JS_PKGS) -I src -I gui -I js_gui js_gui/gui.js

js2:
	ocamlfind ocamlc -package js_of_ocaml -package js_of_ocaml-ppx -linkpkg -o js_gui/gui.byte js_gui/gui.ml
	js_of_ocaml js_gui/gui.byte

opt2:
	$(OCAMLBUILD) -cflags -annot,-bin-annot,'-open Base' -pkg $(PKGS) -tag thread -I src -I gui src/main.native

opt:
	$(OCAMLBUILD) -ocamlc ocamlopt -ocamlopt ocamlopt -tag thread -cflags '-open Base',-unsafe,-annot,-bin-annot,-noassert,-unbox-closures,-unboxed-types,-nodynlink,-O3,-rounds=4,-unbox-closures-factor=100 -pkg $(PKGS) -I src -I gui src/main.native

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
