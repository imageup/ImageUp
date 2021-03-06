{ open Parser }
let digit = ['0' - '9']
let digits = digit+

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "##"     { comment1 lexbuf }    (* NEW *)
| "/#"     { comment lexbuf }     (* NEW Comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| '['      { LSQBRACE }  (* NEW *)
| ']'      { RSQBRACE }   (* NEW *)
| ';'      { SEMI }
| ':'      { COLON } (* NEW *)
| ','      { COMMA }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '|'      { SEPARATOR } (* NEW *)
| '%'      { MOD }    (* NEW *)
| '='      { ASSIGN }
| '>'      { GT }
| '<'      { LT }
| "->"     { ARROW } (* NEW *)
| "=="     { EQ }
| "!="     { NEQ }
| "<="     { LEQ }
| ">="     { GEQ }
| "&&"     { AND }
| "||"     { OR }
| "!"      { NOT }
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }
| "return" { RETURN }
| "int"    { INT }
| "bool"   { BOOL }
| "float"  { FLOAT }
| "function" { FUNCTION } (* NEW *)
| "char"   { CHAR } (* NEW *)
| "string" { STRING } (* NEW??? *)
| "tuple"  { TUPLE } (* NEW *)
| "matrix" { MATRIX } (* NEW *)
| "image"  { IMAGE } (* NEW *)
| "void"   { VOID }
| "stop"   { STOP } (* NEW *)
| "go"     { GO } (* NEW *)
| "true"   { BLIT(true) }
| "false"  { BLIT(false) }
| digits as lxm { LITERAL(int_of_string lxm) }
| digits '.'  digit* ( ['e' 'E'] ['+' '-']? digits )? as lxm { FLIT(lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*     as lxm { ID(lxm) }
| '"' ([^ '"']* as lxm) '"' { STRINGLIT(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "#/" { token lexbuf }
| _    { comment lexbuf }

(* NEW *)
and comment1 = parse   
  "\n"  {token lexbuf}
| _     {comment1 lexbuf}
