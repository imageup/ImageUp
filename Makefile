imageupmake :
	ocamllex scanner.mll
	ocamlyacc parser.mly
	ocamlc -c ast.ml
	ocamlc -c parser.mli
	ocamlc -c scanner.ml
	ocamlc -c parser.ml 
	ocamlc -c imageup.ml
	ocamlc -o imageup parser.cmo scanner.cmo imageup.cmo	

imp :
	ocamlbuild -r -pkgs llvm imageup.native

.PHONY : clean
clean :
	rm -rf ast.cmi ast.cmo parser.cmi parser.cmo scanner.cmi scanner.cmo parser.mli scanner.ml parser.ml	
