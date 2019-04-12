type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq | And | Or  | Mod 

type uop = Neg | Not

type typ = Int | Char | String | Matrix | Image | Tuple | Bool | Float | Void


type bind =  typ * string

type expr =
    Literal of int     (* integer *)
  | Fliteral of string (* float *)
  | Cliteral of char   (* char type *)
  | Sliteral of string (* string type*)
  | BoolLit of bool    (* boolean *)
  | MatLit of expr list list
  | Id of string       (* variable name *)
  | BiTuple of expr * expr (*  *)
  | TriTuple of expr * expr * expr 
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of string * expr (* Matrix : mat(2,2) = [1,2|1,2]; *)
  | MatrixAccess of string * expr * expr (* mat[2][3+i] *) (* expr here must be a tuple of int *)
  | MatAssign of string * expr * expr * expr
  (* | CommaCombine of expr * expr (* 1,2 *) *)
  (* | Separator of expr * expr (* [1,2,3 | 3,2,1] *) *)
  | TupleAccess of string * expr
  | Call of string * expr list
  | Noexpr

  
(* type matbind = typ * string * expr * expr *)
type stmt =
    Block of stmt list
  | DeclAsn of bind * expr
  | TypeAsn of bind
  | MatDeclAsn of typ * string * expr * expr  * expr  (* Matrix : mat(2,2) = [1,2|3,4]; *)
  | MatDecl of typ * string * expr * expr 
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt
  | Break  
  | Conti 


type func_decl = {
    typ : typ;
    fname : string;
    formals : bind list;
    body : stmt list;
  }

type program = bind list * func_decl list

(* Pretty-printing functions *)

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"

let string_of_uop = function
    Neg -> "-"
  | Not -> "!"

let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | Fliteral(l) -> l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | Cliteral(c) -> String.make 1 c
  | Sliteral(s) -> s
  | MatLit (el) -> "[" ^ String.concat "| " (List.map (fun e2 -> String.concat ", " (List.map string_of_expr e2)) el) ^ ";]"
  | BiTuple(e1, e2) -> " ( " ^ string_of_expr e1 ^ "," ^ string_of_expr e2 ^ " ) "
  | TriTuple(e1, e2, e3) -> "(" ^ string_of_expr e1 ^ " , " ^ string_of_expr e2 ^ " , " ^ string_of_expr e3 ^ " ) "
  | Id(s) -> s
  | Binop(e1, o, e2) -> string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | MatAssign(v, e1, e2, e3) -> v ^ "[" ^ string_of_expr e1 ^"][" ^string_of_expr e2 ^"]"^ " = " ^ string_of_expr e3
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  (* | CommaCombine(e1, e2) -> string_of_expr e1 ^ " , " ^ string_of_expr e2 *)
  (* | Separator(e1, e2) -> " [ " ^ string_of_expr e1 ^ " | " ^ string_of_expr e2 ^ " ] "(* [1,2,3 | 3,2,1] *) *)
  | MatrixAccess(s, e1, e2) -> s ^ "[" ^ string_of_expr e1 ^ " ] " ^ " [ " ^ string_of_expr e2 ^ " ] "
  | Call(f, el) -> f ^ " ( " ^ String.concat ", " (List.map string_of_expr el) ^ " ) "
  | TupleAccess(s, e1) -> s ^ "[" ^ string_of_expr e1 ^ "]"
  | Noexpr -> ""

let string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Float -> "float"
  | Void -> "void"
  | Char -> "char"
  | String -> "string"
  | Matrix -> "matrix"
  | Tuple -> "tuple"
  | Image -> "image"

let rec string_of_stmt = function
    Block(stmts) -> "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | DeclAsn((t, s),e) -> string_of_typ t ^ " : " ^ s ^ " = " ^ string_of_expr e ^ ";\n";
  | MatDeclAsn(t, s, e1, e2, e3) -> string_of_typ t ^ ":" ^s ^"("^ string_of_expr e1^","^string_of_expr e2^")"^"=" ^string_of_expr e3^";\n"; 
  | MatDecl(t, s, e1, e2) -> string_of_typ t ^ ":" ^s ^"("^ string_of_expr e1^","^string_of_expr e2^");\n"; 
  | TypeAsn((t, e)) -> string_of_typ t ^ " : " ^ e ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^ string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | Break -> "stop"
  | Conti -> "go"
  

let string_of_vdecl (t, id) =  string_of_typ t ^ " : " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  "function" ^ " " ^ fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^ ")" ^ "->" ^ string_of_typ fdecl.typ  
  ^ "\n{\n" ^ String.concat "" (List.map string_of_stmt fdecl.body) ^ "}\n"

let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)

