# ImageUp
This is the repo for PLT final project


$ ocamllex scanner.mll # create scanner.ml
8 states, 267 transitions, table size 1116 bytes


$ ocamlyacc parser.mly # create parser.ml and parser.mli


$ ocamlc -c ast.mli # compile AST types


$ ocamlc -c parser.mli # compile parser types


$ ocamlc -c scanner.ml # compile the scanner


$ ocamlc -c parser.ml # compile the parser


$ ocamlc -c calc.ml # compile the interpreter


$ ocamlc -o calc parser.cmo scanner.cmo calc.cmo
