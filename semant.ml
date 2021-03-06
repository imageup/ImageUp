(* Semantic checking for the MicroC compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)

(* Semantic checking of the AST. Returns an SAST if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)

let check (globals, functions) =

  (* Verify a list of bindings has no void types or duplicate names *)
  let check_binds (kind : string) (binds : bind list) =
    List.iter (function
	(Void, b) -> raise (Failure ("illegal void " ^ kind ^ " " ^ b))
      | _ -> ()) binds;
    let rec dups = function
        [] -> ()
      |	((_,n1) :: (_,n2) :: _) when n1 = n2 ->
	  raise (Failure ("duplicate " ^ kind ^ " " ^ n1))
      | _ :: t -> dups t
    in dups (List.sort (fun (_,a) (_,b) -> compare a b) binds)
  in

  (**** Check global variables ****)

  check_binds "global" globals;

  (**** Check functions ****)

  (* Collect function declarations for built-in functions: no bodies *)
  let built_in_decls = 
    let add_bind map (name, formals,typee)  = StringMap.add name {
      typ = typee;
      fname = name; 
      formals = formals;
       body = [] } map
    in List.fold_left add_bind StringMap.empty 
    [ 
      ("print", [(Int, "x")], Void);
      ("printb", [(Bool, "x")], Void);
      ("printf", [(Float, "x")], Void);
      ("prints", [(String, "x")], Void);
      ("IntParses", [(String, "x")], Int);
      ("IntParsef", [(Float, "x")], Int);
      ("StrParsef", [(Float, "x")], String);
      ("StrParse", [(Int, "x")], String);
      ("FloatParses", [(String, "x")], Float);
      ("FloatParse", [(Int, "x")], Float);
      ("RowLen", [(Matrix, "matrix")], Int);
      ("ColLen", [(Matrix, "matrix")], Int);
      ("scale", [(Matrix, "matrix"); (Float, "ratio")], Void); 
      ("transpose", [(Matrix, "matrix")], Void);
      ("rotate", [(Matrix, "matrix"); (Bool, "direction")], Void);
      ("multiply", [(Matrix, "matrix1"); (Matrix, "matrix2"); (Matrix, "matrix")], Void); 
      ("read", [(String, "path")], Image);
      ("save", [(String, "path"); (Image, "image")], Void);
      ("get_pixel", [(Image, "image"); (Tuple, "tuple")], Tuple);
      ("write_pixel", [(Image, "image"); (Tuple, "tuple");(Tuple, "tuple")], Void);
      ("smooth", [(Image, "image"); (Float, "ratio")], Image);
      ("adjust_saturation", [(Image, "image"); (Float, "ratio")], Image);
      ("copy", [(Image, "image")], Image);
      ("size", [(Image, "image")], Tuple)
    ]
  in

  (* Add function name to symbol table *)
  let add_func map fd = 
    let built_in_err = "function " ^ fd.fname ^ " may not be defined"
    and dup_err = "duplicate function " ^ fd.fname
    and make_err er = raise (Failure er)
    and n = fd.fname (* Name of the function *)
    in 
    match fd with (* No duplicate functions or redefinitions of built-ins *)
      _ when StringMap.mem n built_in_decls -> make_err built_in_err
    | _ when StringMap.mem n map -> make_err dup_err  
    | _ ->  StringMap.add n fd map 
  in

  (* Collect all function names into one symbol table *)
  let function_decls = List.fold_left add_func built_in_decls functions
  in
  
  (* Return a function from our symbol table *)
  let find_func s = 
    try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let _ = find_func "main" in (* Ensure "main" is defined *)

  let check_function func =
    (* Make sure no formals or locals are void or duplicates *)
    check_binds "formal" func.formals;
    (* print_string("check function func"); *)
    let rec generate_locals = function
      | [] -> []
      | head :: tail -> 
      (
        match head with 
        | DeclAsn((t, n), valuex) -> 
        (
          let tmp = (t, n) in tmp :: generate_locals tail
        )
        | TypeAsn(t, n) -> 
        (
          let tmp = (t, n) in tmp :: generate_locals tail
        )
        | MatDeclAsn(t, n, i, j, valuex) ->
        (
          let tmp = (t, n) in tmp :: generate_locals tail
        )
        | MatDecl (t, n, i, j) ->
        (
          let tmp = (t, n) in tmp :: generate_locals tail
        )
        | _ -> generate_locals tail
      )
    in

(*     let matrix_map =
      let find_matrix_and_add m stmt = 
        match stmt with 
          | MatDeclAsn(t, n, i, j, valuex) -> StringMap.add n (i, j) m
          | MatDecl(t, n, i, j) -> StringMap.add n (i, j) m
          | _ -> m
      in List.fold_left find_matrix_and_add StringMap.empty func.body
    in *)

    let local_vars = generate_locals func.body 
    in
    (* Raise an exception if the given rvalue type cannot be assigned to
       the given lvalue type *)
    let check_assign lvaluet rvaluet err =
       if lvaluet = rvaluet then lvaluet else raise (Failure err)
    in   

    (* Build local symbol table of variables for this function *)
    let symbols = 
    (
      List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
      StringMap.empty (globals @ func.formals @ local_vars)
    )
    in

    (* Return a variable from our local symbol table *)
    let type_of_identifier s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    (* Return a semantically-checked expression, i.e., with a type *)
    let rec expr = function
        Literal  l -> (Int, SLiteral l)
      | Fliteral l -> (Float, SFliteral l)
      | Cliteral l -> (Char, SCliteral l)
      | Sliteral l -> (String, SSliteral l)
      | BoolLit l  -> (Bool, SBoolLit l)
      | Noexpr     -> (Void, SNoexpr)
      | Id s       -> (type_of_identifier s, SId s)
      | BiTuple (e1, e2) -> 
        let (t1, e1') = expr e1 
        and (t2, e2') = expr e2
        in
        (Tuple, SBiTuple ((t1, e1'), (t2, e2')))
      | TriTuple (e1, e2, e3) -> 
        let (t1, e1') = expr e1
        and (t2, e2') = expr e2
        and (t3, e3') = expr e3
        in
        (Tuple, STriTuple ((t1, e1'), (t2, e2'), (t3, e3')))
      | TupleAccess(s, e1) ->
        (
          match e1 with
          | Literal i -> (Float, STupleAccess(s, (Int, SLiteral i)))
          | _ -> raise(Failure("Tuple can only be accessed by integer index"))
        )
      | MatLit el  ->  
        let rec parse_expr = function
          | [] -> []
          | h1 :: t1 -> let tmp = expr h1 in tmp :: parse_expr t1
        in
        let rec parse_outer = function 
          | [[]] -> [[]]
          | [] -> []
          | head :: tail -> let tt = parse_expr head in tt :: parse_outer tail
        in
        let result_t = parse_outer el in
        let len = List.length (List.hd result_t) in
        let m = List.map (fun e -> if List.length e = len then 1 else 0) result_t in
        let all_same = List.fold_left (fun s e -> if e = 1 && s = 1 then 1 else 0) 1 m in
        if List.length el = 0
          then (Matrix, SMatLitDim (result_t, 0, 0))
        else if all_same = 1
          then (Matrix, SMatLitDim (result_t, List.length el, List.length (List.hd el)))
        else raise(Failure("Matrix dimension error: Multiple row dimensions"))
      | MatrixAccess (s, e1, e2) ->
      (
        let e1' = expr e1 
        and e2' = expr e2 in
        (Float, SMatrixAccess (s, e1', e2'))
      )
      | MatAssign (s, e1, e2, e3) ->
      (
        let e1' = expr e1
        and e2' = expr e2
        and e3' = expr e3 in
        (Float, SMatAssign (s, e1', e2', e3'))
      )
      | Assign(var, e) as ex -> 
      (
        let lt = type_of_identifier var
        and (rt, e') = expr e in
        let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^ string_of_typ rt ^ " in " ^ string_of_expr ex
        in (check_assign lt rt err, SAssign(var, (rt, e')))
      )
      | Unop(op, e) as ex -> 
      (
        let (t, e') = expr e in
        let ty = match op with
          Neg when t = Int || t = Float -> t
        | Not when t = Bool -> Bool
        | _ -> raise (Failure ("illegal unary operator " ^ string_of_uop op ^ string_of_typ t ^ " in " ^ string_of_expr ex))
        in (ty, SUnop(op, (t, e')))
      )
      | Binop(e1, op, e2) as e -> 
      (
        let (t1, e1') = expr e1 
        and (t2, e2') = expr e2 in
        (* All binary operators require operands of the same type *)
        let same = t1 = t2 in
        (* Determine expression type based on operator and operand types *)
        let ty = match op with
          Add | Sub | Mult | Div when same && t1 = Int   -> Int
        | Add | Sub | Mult | Div when same && t1 = Float -> Float
        | Add | Sub | Mult | Div when same && t1 = String-> String
        | Equal | Neq            when same               -> Bool
        | Less | Leq | Greater | Geq
                   when same && (t1 = Int || t1 = Float) -> Bool
        | And | Or when same && t1 = Bool -> Bool
        | _ -> raise (Failure ("illegal binary operator " ^
                     string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                     string_of_typ t2 ^ " in " ^ string_of_expr e))
        in (ty, SBinop((t1, e1'), op, (t2, e2')))
      )
      | Call(fname, args) as call -> 
      (
        let fd = find_func fname in
        let param_length = List.length fd.formals in
        if List.length args != param_length then
          raise (Failure ("expecting " ^ string_of_int param_length ^ " arguments in " ^ string_of_expr call))
        else 
          let check_call (ft, _) e = 
          let (et, e') = expr e in 
          let err = "illegal argument found " ^ string_of_typ et ^ " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
          in (check_assign ft et err, e')
        in 
        let args' = List.map2 check_call fd.formals args
        in (fd.typ, SCall(fname, args'))
      )
    in

    let check_bool_expr e = 
    (
      let (t', e') = expr e
      and err = "expected Boolean expression in " ^ string_of_expr e
      in if t' != Bool then raise (Failure err) else (t', e') 
    )
    in

    (* Return a semantically-checked statement i.e. containing sexprs *)
    let rec check_stmt = function
        Expr e -> SExpr (expr e)
      | If(p, b1, b2) -> SIf(check_bool_expr p, check_stmt b1, check_stmt b2)
      | DeclAsn((t, s), e) -> 
      (
        let (rt, e') = expr e in
        let err = "illegal assignment " ^ string_of_typ t ^ " = " ^ 
          string_of_typ rt ^ " in " ^ string_of_expr e
        in if rt = t then SDeclAsn((t, s), expr e)
        else raise(Failure err)
      )
      | TypeAsn((t, e)) -> STypeAsn((t, e))
      | Break -> SBreak
      | Conti -> SConti
      | MatDecl(ty, s, e1, e2) -> SMatDecl(ty, s, expr e1, expr e2)
      | MatDeclAsn(ty, s, e1, e2, e3) -> 
      (
        match expr e3 with
        |(Matrix, SMatLitDim (_, r, c))  -> 
        (
          let d1 = SBinop(expr e1, Mult, expr e2) in
          let r' = (Int, SLiteral r) in
          let c' = (Int, SLiteral c) in
          let d2 = SBinop(r', Mult, c') in
          if d1 = d2 then SMatDeclAsn(ty, s, expr e1, expr e2, expr e3)
          else raise(Failure("Matrix error: Declared dimension does not match with actual matrix's dimension"))
        )
        |_ -> raise(Failure("Illegal Matrix declaration format"))
      )
      | For(e1, e2, e3, st) -> SFor(expr e1, check_bool_expr e2, expr e3, check_stmt st)
      | While(p, s) -> SWhile(check_bool_expr p, check_stmt s)
      | Return e -> 
      (
        let (t, e') = expr e in
        if t = func.typ then SReturn (t, e') 
        else raise ( Failure ("return gives " ^ string_of_typ t ^ " expected " ^string_of_typ func.typ ^ " in " ^ string_of_expr e))
      )
	    (* A block is correct if each statement is correct and nothing
	       follows any Return statement.  Nested blocks are flattened. *)
      | Block sl -> 
      (
        let rec check_stmt_list = function
            [Return _ as s] -> [check_stmt s]
          | Return _ :: _   -> raise (Failure "nothing may follow a return")
          | Block sl :: ss  -> check_stmt_list (sl @ ss) (* Flatten blocks *)
          | s :: ss         -> check_stmt s :: check_stmt_list ss
          | []              -> []
        in SBlock(check_stmt_list sl)
      )

    in (* body of check_function *)
    { styp = func.typ;
      sfname = func.fname;
      sformals = func.formals;
      sbody = 
      (
        match check_stmt (Block func.body) with
  	    | SBlock(sl) -> sl
        | _ -> raise (Failure ("internal error: block didn't become a block?"))
      )
    }
  in (globals, List.map check_function functions)
