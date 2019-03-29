%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA COLON LSQBRACE RSQBRACE LPERCENT RPERCENT SEPARATOR EOF FUNCTION
%token PLUS MINUS TIMES DIVIDE MOD ASSIGN ARROW NOT 
%token EQ NEQ LT LEQ GT GEQ AND OR 
%token TRUE FALSE
%token RETURN STOP GO IF ELSE FOR WHILE INT BOOL VOID FLOAT TUPLE STRING CHAR MATRIX IMAGE

%token <int> LITERAL
%token <char> CHARLIT
%token <string> STRINGLIT 
%token <string> ID
%token <float> FLIT
%token <bool> BLIT

%start program
%type <Ast.program> program

%nonassoc NOELSE
%nonassoc ELSE
%left SEMI
%right ARROW
%right COLON
%left RSQBRACE
%right LSQBRACE
%left SEPARATOR
%left RPAREN
%right LPAREN
%left COMMA
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE MOD
%right NOT 

%%

program:
  decls EOF { $1 }

decls:
   /* nothing */ { ([], [])               }
 | decls vdecl { (($2 :: fst $1), snd $1) }
 | decls fdecl { (fst $1, ($2 :: snd $1)) }

fdecl:
    FUNCTION ID LPAREN formals_opt RPAREN ARROW typ LBRACE stmt_list RBRACE
     { { typ = $7;
   fname = $2;
   formals = List.rev $4;
   body = List.rev $9 } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { $1 }

formal_list:
    /*ID COLON typ                   { [($1, $3)]     }*/
    typ COLON ID                   { [($1, $3)]     }
  | formal_list COMMA typ COLON ID { ($3, $5) :: $1 }
  /*| formal_list COMMA ID COLON typ { ($3, $5) :: $1 }*/

typ:
    INT     { Int   }
  | BOOL    { Bool  }
  | FLOAT   { Float }
  | VOID    { Void  }
  | CHAR    { Char  }
  | STRING  { String}
  | TUPLE   { Tuple }
  | MATRIX  { Matrix}
  | IMAGE   { Image }


vdecl:
   /*ID COLON typ SEMI { ($1, $3) }*/
   typ COLON ID SEMI { ($1, $3) }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
  | expr SEMI                               { Expr $1               }
  | typ COLON ID ASSIGN expr SEMI           { DeclAsn(($1, $3), $5) }
  | typ COLON ID SEMI                       { TypeAsn($1, $3) }
  | RETURN expr_opt SEMI                    { Return $2             }
  | LBRACE stmt_list RBRACE                 { Block(List.rev $2)    }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7)        }
  | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt { For($3, $5, $7, $9)   }
  | WHILE LPAREN expr RPAREN stmt           { While($3, $5)         }
  | STOP SEMI                               { Break }
  | GO SEMI                                 { Conti }
  

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }
  

expr:
    LITERAL               { Literal($1)            }
  | FLIT                  { Fliteral($1)           }
  | BLIT                  { BoolLit($1)            }
  | CHARLIT               { Cliteral($1)           }
  | STRINGLIT             { Sliteral($1)           }
  | ID                    { Id($1)                 }
  | expr PLUS   expr      { Binop($1, Add,   $3)   }
  | expr MINUS  expr      { Binop($1, Sub,   $3)   }
  | expr TIMES  expr      { Binop($1, Mult,  $3)   }
  | expr DIVIDE expr      { Binop($1, Div,   $3)   }
  | expr EQ     expr      { Binop($1, Equal, $3)   }
  | expr NEQ    expr      { Binop($1, Neq,   $3)   }
  | expr LT     expr      { Binop($1, Less,  $3)   }
  | expr LEQ    expr      { Binop($1, Leq,   $3)   }
  | expr GT     expr      { Binop($1, Greater, $3) }
  | expr GEQ    expr      { Binop($1, Geq,   $3)   }
  | expr AND    expr      { Binop($1, And,   $3)   }
  | expr OR     expr      { Binop($1, Or,    $3)   }
  | expr MOD    expr      { Binop($1, Mod,   $3)   }
  | MINUS expr %prec NOT  { Unop(Neg, $2)          }
  | NOT expr              { Unop(Not, $2)          }
  | ID ASSIGN expr      { Assign($1, $3)         }
  | ID LSQBRACE expr RSQBRACE LSQBRACE expr RSQBRACE ASSIGN expr      { MatAssign($1, $3, $6, $9)         }
  | LPAREN expr RPAREN    { $2                     }
  | expr COMMA expr       { CommaCombine($1, $3)   }
  | expr SEPARATOR expr   { Separator($1, $3)      }
  | LPAREN expr COMMA expr RPAREN { BiTuple($2, $4)}
  | LPAREN expr COMMA expr COMMA expr RPAREN { TriTuple($2, $4, $6) }
  | ID LSQBRACE expr RSQBRACE LSQBRACE expr RSQBRACE {MatrixAccess($1, $3, $6)}
  | LSQBRACE expr RSQBRACE    { $2                     }
/*
tuple_typ:
    typ LPERCENT LITERAL RPERCENT { TupleTyp($1, $3) }

args_opt:
    *//* nothing */ /*{ [] }
  | args_list  { List.rev $1 }

args_list:
    expr                    { [$1] }
  | args_list COMMA expr { $3 :: $1 }

*/
