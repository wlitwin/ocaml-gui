#export OCAMLFIND_IGNORE_DUPS_IN=/home/walter/.opam/4.04.0/lib/ocaml/compiler-libs

OCAMLBUILD = ocamlbuild -use-ocamlfind

PKGS=cairo2,cairo2.lablgtk2,lablgtk2,lablgtk2-extras,base,extlib,core
#,ocamlgraph,ocamlgraph.dgraph
JS_PKGS=base,js_of_ocaml,js_of_ocaml-ocamlbuild,js_of_ocaml-ppx

COMMON_INCLUDES=-I gui/ -I backends/sigs/

dbg:
	$(OCAMLBUILD) -cflags -annot,-bin-annot,'-open Base' -pkg $(PKGS) -tag thread $(COMMON_INCLUDES) -I backends/gtk -I src src/main.d.byte

js:
	$(OCAMLBUILD) -plugin-tag "package(js_of_ocaml.ocamlbuild)" -cflags -annot,-bin-annot,'-open Base' -pkg $(JS_PKGS) $(COMMON_INCLUDES) -tag debug -I backends/canvas/ -I js_gui js_gui/main.js

JS_TARGET=main

js2:
	ocamlfind ocamlc -open Base -annot -bin-annot -package base -package js_of_ocaml -package js_of_ocaml-ppx -thread -ppx -linkpkg -o js_gui/$(JS_TARGET).byte js_gui/$(JS_TARGET).ml
	js_of_ocaml js_gui/$(JS_TARGET).byte
	mv js_gui/*.annot _build/js_gui/.
	mv js_gui/*.binannot _build/js_gui/.
	mv js_gui/*.cmo _build/js_gui/.
	mv js_gui/*.cmi _build/js_gui/.
	mv js_gui/*.byte _build/js_gui/.
	mv js_gui/*.js _build/js_gui/.

run_js:
	firefox js_gui/index.html

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
