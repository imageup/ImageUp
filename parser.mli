type token =
  | SEMI
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | COMMA
  | COLON
  | LSQBRACE
  | RSQBRACE
  | LPERCENT
  | RPERCENT
  | SEPARATOR
  | EOF
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | MOD
  | ASSIGN
  | ARROW
  | NOT
  | EQ
  | NEQ
  | LT
  | LEQ
  | GT
  | GEQ
  | AND
  | OR
  | TRUE
  | FALSE
  | RETURN
  | STOP
  | GO
  | IF
  | ELSE
  | FOR
  | WHILE
  | INT
  | BOOL
  | VOID
  | FLOAT
  | TUPLE
  | STRING
  | CHAR
  | MATRIX
  | IMAGE
  | LITERAL of (int)
  | CHARLIT of (char)
  | STRINGLIT of (string)
  | ID of (string)
  | FLIT of (float)
  | BLIT of (bool)

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
