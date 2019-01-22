#export OCAMLFIND_IGNORE_DUPS_IN=/home/walter/.opam/4.04.0/lib/ocaml/compiler-libs

OCAMLBUILD = ocamlbuild -use-ocamlfind

PKGS=cairo2,cairo2.lablgtk2,lablgtk2,base,core
#,ocamlgraph,ocamlgraph.dgraph
JS_PKGS=base,js_of_ocaml,js_of_ocaml-ocamlbuild,js_of_ocaml-ppx

COMMON_INCLUDES=-I apps/ -I gui/ -I backends/sigs/ -I gui/layout/

SPACETIME=-ocamlopt /home/walter/code/ocaml/build/bin/ocamlopt

dbg:
	$(OCAMLBUILD) -cflags -g,-annot,-bin-annot,'-open Base' -pkg $(PKGS) -tag thread $(COMMON_INCLUDES) -I backends/gtk -I src src/main.d.byte

js:
	$(OCAMLBUILD) -plugin-tag "package(js_of_ocaml.ocamlbuild)" -cflags -annot,-bin-annot,'-open Base' -pkg $(JS_PKGS) $(COMMON_INCLUDES) -tag 'opt(3)' -I backends/canvas/ src/main.js

js_debug:
	$(OCAMLBUILD) -plugin-tag "package(js_of_ocaml.ocamlbuild)" -cflags -annot,-bin-annot,'-open Base' -pkg $(JS_PKGS) $(COMMON_INCLUDES) -tag debug -I backends/canvas/ src/main.js

run_js:
	firefox js_gui/index.html

opt2:
	$(OCAMLBUILD) -cflags -annot,-bin-annot,'-open Base' -pkg $(PKGS) -tag thread -I src -I gui src/main.native

opt:
	$(OCAMLBUILD) -ocamlc ocamlopt -ocamlopt ocamlopt -cflags -annot,-bin-annot,'-open Base',-unsafe,-noassert,-nodynlink,-unbox-closures,-unboxed-types,-O3,-rounds=4,-unbox-closures-factor=100 -pkg $(PKGS) -tag thread $(COMMON_INCLUDES) -I backends/gtk -I src src/main.native

prof:
	$(OCAMLBUILD) -cflags -annot,-bin-annot,'-open Base' -pkg $(PKGS) -tag debug -tag thread -tag thread $(COMMON_INCLUDES) -I backends/gtk -I src src/main.native

perf: prof
	sudo perf record -g --call-graph=dwarf ./main.native

report:
	sudo perf report -G --tui

debugger: dbg
	oqamldebug ./main.d.byte

test: all
	$(OCAMLBUILD) -pkg $(PKGS),oUnit -Is src,test test/test.native
	./test.native

clean:
	$(OCAMLBUILD) -clean
