(* Semantically-checked Abstract Syntax Tree and functions for printing it *)

open Ast

type sexpr = typ * sx
and sx =
    SLiteral of int
  | SFliteral of string
  | SSliteral of string
  | SCliteral of char
  | SBoolLit of bool
  | SMatlit of sexpr list list
  | SId of string
  | SBiTuple of sexpr * sexpr
  | STriTuple of sexpr * sexpr * sexpr
  | SBinop of sexpr * op * sexpr
  | SUnop of uop * sexpr
  | SAssign of string * sexpr
  | SMatAssign of string * sexpr * sexpr * sexpr
  | SCommaCombine of sexpr * sexpr
  | SMatrixAccess of string * sexpr * sexpr
  | SSeparator of sexpr * sexpr 
  | STupleAccess of string * sexpr
  | SCall of string * sexpr list
  | SNoexpr
  | SMatLit of sexpr list list

type sstmt =
    SBlock of sstmt list
  | SExpr of sexpr
  | SDeclAsn of bind * sexpr
  | SMatDeclAsn of matbind * expr  (* Matrix : mat(2,2) = [1,2|3,4]; *)
  | STypeAsn of bind
  | SReturn of sexpr
  | SIf of sexpr * sstmt * sstmt
  | SFor of sexpr * sexpr * sexpr * sstmt
  | SWhile of sexpr * sstmt
  | SBreak  
  | SConti  

type sfunc_decl = {
    styp : typ;
    sfname : string;
    sformals : bind list;
    sbody : sstmt list;
  }

type sprogram = bind list * sfunc_decl list

(* Pretty-printing functions *)

let rec string_of_sexpr (t, e) =
  "(" ^ string_of_typ t ^ " : " ^ (match e with
    SLiteral(l) -> string_of_int l
  | SBoolLit(true) -> "true"
  | SBoolLit(false) -> "false"
  | SCliteral(c) -> String.make 1 c
  | SSliteral(s) -> s
  | SMatLit (el) -> "[" ^ String.concat "; " (List.map (fun e2 -> String.concat ", " (List.map string_of_sexpr e2)) el) ^ ";]"
  | SBiTuple(e1, e2) -> "(" ^ string_of_sexpr e1 ^ "," ^ string_of_sexpr e2 ^ ")"
  | STriTuple(e1, e2, e3) -> "(" ^ string_of_sexpr e1 ^","^ string_of_sexpr e2 ^","^
        string_of_sexpr e3^")"
  | SFliteral(l) -> l
  | SId(s) -> s
  | SBinop(e1, o, e2) ->
      string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
  | SUnop(o, e) -> string_of_uop o ^ string_of_sexpr e
  | STupleAccess (s, e1) -> s ^ "[" ^ string_of_sexpr e1 ^ "]"
  | SMatAssign(v, e1, e2, e3) -> v ^ "[" ^ string_of_sexpr e1 ^"][" ^string_of_sexpr e2 ^"]"^ " = " ^ string_of_sexpr e3
  | SAssign(v, e) -> v ^ " = " ^ string_of_sexpr e
  | SCommaCombine(e1, e2) -> string_of_sexpr e1 ^ " , " ^ string_of_sexpr e2
  | SSeparator(e1, e2) -> " [ " ^ string_of_sexpr e1 ^ " | " ^ string_of_sexpr e2 ^ " ] "(* [1,2,3 | 3,2,1] *)
  | SMatrixAccess(s, e1, e2) -> s ^ "[" ^ string_of_sexpr e1 ^ " ] " ^ " [ " ^ string_of_sexpr e2 ^ " ] "
  | SCall(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
  | SNoexpr -> ""
          ) ^ ")"             

let rec string_of_sstmt = function
    SBlock(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n"
  | SExpr(expr) -> string_of_sexpr expr ^ ";\n";
  | SReturn(expr) -> "return " ^ string_of_sexpr expr ^ ";\n";
  | SIf(e, s, SBlock([])) ->
      "if (" ^ string_of_sexpr e ^ ")\n" ^ string_of_sstmt s
  | SDeclAsn((t, s),e) -> string_of_typ t ^ " : " ^ s ^ " = " ^ string_of_sexpr e ^ ";\n"
  | STypeAsn((t, e)) -> string_of_typ t ^ " : " ^ e ^ ";\n"
  | SIf(e, s1, s2) ->  "if (" ^ string_of_sexpr e ^ ")\n" ^
      string_of_sstmt s1 ^ "else\n" ^ string_of_sstmt s2
  | SFor(e1, e2, e3, s) ->
      "for (" ^ string_of_sexpr e1  ^ " ; " ^ string_of_sexpr e2 ^ " ; " ^
      string_of_sexpr e3  ^ ") " ^ string_of_sstmt s
  | SWhile(e, s) -> "while (" ^ string_of_sexpr e ^ ") " ^ string_of_sstmt s
  | SBreak -> "stop"
  | SConti -> "go"

let string_of_sfdecl fdecl =
  "function" ^ " " ^ fdecl.sfname ^ "(" ^ String.concat ", " (List.map snd fdecl.sformals)     ^ ")" ^ "->" ^ string_of_typ fdecl.styp
  ^ "\n{\n" ^ String.concat "" (List.map string_of_sstmt fdecl.sbody) ^ "}\n"


let string_of_sprogram (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_sfdecl funcs)
