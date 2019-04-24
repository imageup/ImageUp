imageupmake :
	ocamllex scanner.mll
	ocamlyacc parser.mly
	ocamlc -c ast.ml
	ocamlc -c parser.mli
	ocamlc -c scanner.ml
	ocamlc -c parser.ml 
	ocamlc -c imageup.ml
	ocamlc -o imageup parser.cmo scanner.cmo imageup.cmo	
test :
	./imageup.native test.iu > test.ll; llc -relocation-model=pic test.ll > test.s ;gcc -o test.exe test.s lib.c `pkg-config --cflags --libs opencv`;./test.exe

testing :
	llc -relocation-model=pic test.ll > test.s ;gcc -o test.exe test.s lib.c `pkg-config --cflags --libs opencv`;./test.exe

t_adj_img:
	./imageup.native test_adjust_image.iu > test.ll; llc -relocation-model=pic test.ll > test.s ;gcc -o test.exe test.s lib.c `pkg-config --cflags --libs opencv`;./test.exe
imp :
	ocamlbuild -r -pkgs llvm -pkgs llvm.analysis imageup.native

.PHONY : clean
clean :
	rm -rf ast.cmi ast.cmo parser.cmi parser.cmo scanner.cmi scanner.cmo parser.mli scanner.ml parser.ml test.ll test.s imageup.native	
