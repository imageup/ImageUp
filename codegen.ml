(* IMPORTANT!!!!! *)
(* check bituple or trituple *)
(* matrix index & check is needed *)
(* matrix 200 * 200 store memory junk *)
(* need to add global matrix in function formal as function parameter *)

module L = Llvm
module A = Ast
open Sast 

module StringMap = Map.Make(String)

(* translate : Sast.program -> Llvm.module *)
let translate (globals, functions) =
  let context    = L.global_context () in
  let image_size = 30 in 
  
  (* Create the LLVM compilation module into which
     we will generate code *)
  let the_module = L.create_module context "ImageUp" in

  (* Get types from the context *)
  let i32_t      = L.i32_type    context
  and i8_t       = L.i8_type     context
  and i1_t       = L.i1_type     context
  and float_t    = L.double_type context
  and string_t   = L.pointer_type (L.i8_type context) 
  and array_t n  = (L.array_type (L.double_type context) n)
  and matrix_t m n = (L.array_type (L.array_type (L.double_type context) n) m)
  and void_t     = L.void_type   context
  in let image_t = L.named_struct_type context "image_t"
  in L.struct_set_body image_t [| (L.array_type (L.array_type float_t image_size) image_size );
                                  (L.array_type (L.array_type float_t image_size) image_size );
                                  (L.array_type (L.array_type float_t image_size) image_size ) |] false;

  (* Return the LLVM type for a MicroC type *)
  let ltype_of_typ = function
      A.Int   -> i32_t
    | A.Bool  -> i1_t
    | A.Float -> float_t
    | A.Void  -> void_t
    | A.Char  -> i8_t
    | A.String -> string_t
    | A.Tuple -> L.array_type float_t 3

    | A.Matrix -> L.pointer_type (matrix_t 200 200)
    | A.Image -> L.pointer_type image_t
  in
(* 

type typ = Int | Char | String | Matrix | Image | Tuple | Bool | Float | Void

 *)
  (* Create a map of global variables after creating each *)
  let global_vars : L.llvalue StringMap.t =
    let global_var m (t, n) = 
      let init = match t with
          A.Float -> L.const_float (ltype_of_typ t) 0.0
        | _ -> L.const_int (ltype_of_typ t) 0
      in StringMap.add n (L.define_global n init the_module) m in
      List.fold_left global_var StringMap.empty globals in

      let printf_t : L.lltype = 
          L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
      let printf_func : L.llvalue = 
          L.declare_function "printf" printf_t the_module in
      (* let prints_t : L.lltype = 
	  L.function_type string_t [| string_t; string_t |] in   *)
(*       let prints_func : L.llvalue =
	  L.declare_function "prints" prints_t the_module in *)
	  
(*       let printbig_t : L.lltype =
          L.function_type i32_t [| i32_t |] in
      let printbig_func : L.llvalue =
          L.declare_function "printbig" printbig_t the_module in *)

  (* Define each function (arguments and return type) so we can 
     call it even before we've created its body *)
  let function_decls : (L.llvalue * sfunc_decl) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname
      and formal_types = Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sformals)
      in 
      let ftype = L.function_type (ltype_of_typ fdecl.styp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
      List.fold_left function_decl StringMap.empty functions in
  
  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder
    and float_format_str = L.build_global_stringptr "%g\n" "fmt" builder 
    and	string_format_str = L.build_global_stringptr "%s\n" "fmt" builder in

  let rec range i j = if i > j then [] else i :: (range (i+1) j) in

  (*let matrix_map =
    let find_matrix_and_add m stmt = 
      match stmt with 
        | SMatDeclAsn(t, n, i, j, valuex) -> StringMap.add n ((L.const_int i32_t i), (L.const_int i32_t j)) m
        | SMatDecl(t, n, i, j) -> StringMap.add n ((L.const_int i32_t i), (L.const_int i32_t j)) m
        | _ -> m
    in List.fold_left find_matrix_and_add StringMap.empty fdecl.sbody
  in*)
  (* Construct the function's "locals": formal arguments and locally
     declared variables.  Allocate each on the stack, initialize their
     value, if appropriate, and remember their values in the "locals" map *)
  let local_vars =
    let add_formal m (t, n) p = 
      L.set_value_name n p;
    let local = L.build_alloca (ltype_of_typ t) n builder in
      ignore (L.build_store p local builder);
      StringMap.add n local m 

    (* Allocate space for any locally declared variables and add the
     * resulting registers to our map *)
    and add_local m stmt =
      match stmt with 
      | STypeAsn(t, n) ->
      (
        let local = L.build_alloca (ltype_of_typ t) n builder in StringMap.add n local m
      )
      | SDeclAsn((t, n), valuex) -> 
      (
        let local = L.build_alloca (ltype_of_typ t) n builder in StringMap.add n local m
      )
      | SMatDeclAsn(t, n, i, j, valuex) ->
      (
        let local = L.build_alloca (ltype_of_typ t) n builder in StringMap.add n local m
      )
      | _ -> m
    in

    let formals = List.fold_left2 add_formal StringMap.empty fdecl.sformals
        (Array.to_list (L.params the_function)) 
    in
    List.fold_left add_local formals fdecl.sbody
    in

    (* Return the value for a variable or formal argument.
       Check local names first, then global names *)
    let lookup n = try StringMap.find n local_vars
                   with Not_found -> StringMap.find n global_vars


    in
    let load_image name load_return matrix_map image_map builder =
      let img = L.build_malloc image_t "new_img" builder
      in let red_ptr = L.build_struct_gep img 0 "red_ptr" builder
      and green_ptr = L.build_struct_gep img 1 "green_ptr" builder
      and blue_ptr = L.build_struct_gep img 2 "blue_ptr" builder

      in let rec grub_image iter =
        let size = 3 * image_size * image_size + 2

        in (match iter with
          | n when n = 0 -> (grub_image (iter + 1))
          | n when n = 1 -> (grub_image (iter + 1))
          | n when n = size -> []
          | _ -> let next_element = (L.build_load (L.build_gep load_return
                [|L.const_int i32_t iter|] "element_ptr" builder) "element" builder)
                in
                (* FILL IN MATRICES *)
                  let mat_list_ix = ((iter-2) / 3)
 
                  in let curr_row = mat_list_ix / image_size
                  in let curr_col = (mat_list_ix mod image_size)
                    in if iter mod 3 = 1
                      then (* FILL IN RED MATRIX *) let red_elem_ptr =
                          L.build_gep red_ptr [| L.const_int i32_t 0;
                          L.const_int i32_t curr_row; L.const_int i32_t curr_col |] "red_mat_ptr" builder
                          in ignore(L.build_store next_element red_elem_ptr builder);
                    else if iter mod 3 = 0
                      then (* FILL IN GREEN MATRIX *) let green_elem_ptr =
                          L.build_gep green_ptr [| L.const_int i32_t 0;
                          L.const_int i32_t curr_row; L.const_int i32_t curr_col |] "green_mat_ptr" builder
                      in ignore(L.build_store next_element green_elem_ptr builder);
                    else if iter mod 3 = 2
                      then (* FILL IN BLUE MATRIX *) let blue_elem_ptr =
                          L.build_gep blue_ptr [| L.const_int i32_t 0;
                          L.const_int i32_t curr_row; L.const_int i32_t curr_col |] "green_mat_ptr" builder
                      in ignore(L.build_store next_element blue_elem_ptr builder);
                    else raise (Failure "Internal error"); (* END ENTIRE IF BLOCK *)
                  
                  (grub_image (iter + 1))
        )
        in ignore(grub_image 0); 

        let new_row_size = L.build_load (L.build_gep load_return [|L.const_int i32_t 0|] "row_size" builder) "number" builder
        in let new_col_size = L.build_load (L.build_gep load_return [|L.const_int i32_t 1|] "col_size" builder) "number" builder 
        in let size = (new_row_size, new_col_size) 
        in let new_image_map = StringMap.add name size image_map
        in (img, (builder, (matrix_map, new_image_map)))
      
    in
    let read_body name args builder matrix_map image_map=

      let path = match args with mm::_ -> match mm with (_, SSliteral s) -> L.build_global_stringptr s "path_name" builder

        
      in let func_def_read = L.function_type (L.pointer_type float_t) [| string_t |]
      in let func_decl_read = L.declare_function "read_c" func_def_read the_module

      in let read_return = L.build_call func_decl_read [| path|] "" builder
      in load_image name read_return matrix_map image_map builder
      (*in (builder, (matrix_map, image_map))*)
    in    
    let save_body args builder matrix_map image_map=
      let path = match args with mm::_ -> match mm with (_, SSliteral s) -> L.build_global_stringptr s "path_name" builder
      in let timg = match args with _::rimg -> List.hd rimg
      in let img = match timg with (_, SId s) -> L.build_load (lookup s) s builder
    in let img_ptr = L.build_gep img [|L.const_int i32_t 0|] "img_ptr" builder
      in let pointer_to_red = L.build_struct_gep img_ptr 0 "i_red" builder

      in let pointer_to_blue = L.build_struct_gep img_ptr 2 "i_blue" builder

      in let pointer_to_green = L.build_struct_gep img_ptr 1 "i_green" builder


      in let red_mat_ptr = L.build_gep pointer_to_red [| L.const_int i32_t 0; L.const_int i32_t 0 |] "ptr_red" builder
      in let blue_mat_ptr = L.build_gep pointer_to_blue [| L.const_int i32_t 0; L.const_int i32_t 0 |] "ptr_blue" builder
      in let green_mat_ptr = L.build_gep pointer_to_green [| L.const_int i32_t 0; L.const_int i32_t 0 |] "ptr_green" builder

      in let ptr_typ = L.pointer_type (array_t image_size)
      in let func_def_save = L.function_type void_t [| string_t; ptr_typ; ptr_typ; ptr_typ|]
      in let func_decl_save = L.declare_function "save_c" func_def_save the_module

      in ignore(L.build_call func_decl_save [| path; red_mat_ptr; green_mat_ptr; blue_mat_ptr|] "" builder);
      
      (L.const_int i32_t 0, (matrix_map, image_map))
    in                                              
    (* Construct code for an expression; return its value *)
    let rec expr (builder, (matrix_map, image_map)) ((_, e) : sexpr) = match e with
      SLiteral i  -> (L.const_int i32_t i, (matrix_map, image_map))
    | SCliteral c -> (L.build_global_stringptr (String.make 1 c) "system_string" builder, (matrix_map, image_map))
    | SBoolLit b  -> (L.const_int i1_t (if b then 1 else 0), (matrix_map, image_map))
    | SFliteral l -> (L.const_float_of_string float_t l, (matrix_map, image_map))
    | SSliteral s -> (L.build_global_stringptr s "system_string" builder, (matrix_map, image_map))
    | SNoexpr     -> (L.const_int i32_t 0, (matrix_map, image_map))
    | SId s       -> (L.build_load (lookup s) s builder, (matrix_map, image_map))
    | SBiTuple ((s1, e1), (s2, e2)) -> 
    (
    (* only int or float, not 3 + 2 *)
      match (e1, e2) with 
        | (SLiteral i, SLiteral j) -> 
          let e1_t = L.const_float_of_string float_t (string_of_int i)
          and e2_t = L.const_float_of_string float_t (string_of_int j)
          and e3_t = L.const_float_of_string float_t (string_of_int 0)
          in (L.const_array float_t (Array.of_list(e1_t::e2_t::[e3_t])), (matrix_map, image_map))
        | (SFliteral i, SFliteral j) ->
          let e1_t = L.const_float_of_string float_t i
          and e2_t = L.const_float_of_string float_t j
          and e3_t = L.const_float_of_string float_t "0.0"
          in (L.const_array float_t (Array.of_list(e1_t::e2_t::[e3_t])), (matrix_map, image_map))
        | _ -> raise(Failure ("only suppurt int or float tuple"))
    )
    | STriTuple((s1, e1), (s2, e2), (s3, e3)) ->
  	(
  		match (e1, e2, e3) with
  		| (SLiteral i, SLiteral j, SLiteral k) ->
        let e1' = L.const_float_of_string float_t (string_of_int i)
        and e2' = L.const_float_of_string float_t (string_of_int j)
        and e3' = L.const_float_of_string float_t (string_of_int k)
  			in (L.const_array float_t (Array.of_list(e1'::e2'::[e3'])), (matrix_map, image_map))
  		| (SFliteral i, SFliteral j, SFliteral k) ->
  			let e1' = L.const_float_of_string float_t i
  			and e2' = L.const_float_of_string float_t j
  			and e3' = L.const_float_of_string float_t k
  			in (L.const_array float_t (Array.of_list(e1'::e2'::[e3'])), (matrix_map, image_map))
  		| _ -> raise(Failure ("only support int or float tuple"))
  	)
    | STupleAccess(s, (s1, SLiteral l)) ->
    (
      (* let s' = L.build_load (lookup s) s builder in *)
      let value = StringMap.find s local_vars in
      (L.build_load (L.build_gep (value) [| L.const_int i32_t 0; L.const_int i32_t l|] s builder) s builder, (matrix_map, image_map))
    )
    | SMatLitDim (s, m, n) -> 
    (
      (* let m = row and n = Array.length (Array.get s 0) in *)
      let rec recompute_in = function
        | [] -> []
        | head :: tail -> let res = fst (expr (builder, (matrix_map, image_map)) head) in res :: recompute_in tail 
      in 
      let rec recompute_out = function
        | [[]] -> let x = (Array.of_list([])) in [x]
        | head::tail -> 
        (
          (* inner *)
          let tmp = Array.of_list(recompute_in head) in 
          tmp :: recompute_out tail
        )
        | [] -> []
      in
      let arr_in = Array.of_list(recompute_out s) in
      let matrix = L.build_malloc (matrix_t 200 200) "res" builder in
      let row_idxs = range 0 (m - 1) in
      let col_idxs = range 0 (n-1) in
      List.iter 
      (
        fun row_idx -> List.iter (
          fun idx -> ignore
          (
            L.build_store 
            (Array.get (Array.get arr_in row_idx) idx)
            (L.build_gep matrix [|L.const_int i32_t 0; L.const_int i32_t row_idx; L.const_int i32_t idx|]  "tmp" builder)  
            builder
          )
        ) col_idxs; 
      ) row_idxs; 
      (matrix, (matrix_map, image_map))
    )
    | SMatrixAccess (s, e1, e2) -> 
    (
      let (i, j) = StringMap.find s matrix_map
      and row_t = fst (expr (builder, (matrix_map, image_map)) e1)
      and col_t = fst (expr (builder, (matrix_map, image_map)) e2)
      in
      let matrix_var = L.build_load (lookup s) s builder in
      if (L.int64_of_const i <= L.int64_of_const row_t) || (L.int64_of_const j <= L.int64_of_const col_t)
      then raise(Failure("matrix index access out of boundary 2"))
      else
      (
        (L.build_load (L.build_gep (matrix_var) [| L.const_int i32_t 0; row_t; col_t|] s builder) s builder, (matrix_map, image_map))
      )
    )
    | SMatAssign (s, e1, e2, e3) ->
    (
      let e3' = fst (expr (builder, (matrix_map, image_map)) e3) in
(*       let get_value val_in = 
        match val_in with
        | Some l -> l
        | None -> raise(Failure("fail to get value"))
      in *)
      let (i, j) = StringMap.find s matrix_map
      and row_t = fst (expr (builder, (matrix_map, image_map)) e1)
      and col_t = fst (expr (builder, (matrix_map, image_map)) e2)
      in
      let matrix_var = L.build_load (lookup s) s builder in
(*       let i_v = get_value (L.int64_of_const i)
      and j_v = get_value (L.int64_of_const j)
      and row_v = get_value (L.int64_of_const row_t)
      and col_v = get_value (L.int64_of_const col_t)
      in *)
      if (L.int64_of_const i <= L.int64_of_const row_t) || (L.int64_of_const j <= L.int64_of_const col_t)
      then raise(Failure("matrix index access out of boundary 1"))
      else
      (
        ignore(L.build_store e3' (L.build_gep (matrix_var) [| L.const_int i32_t 0; row_t; col_t|] s builder) builder);
        (e3', (matrix_map, image_map))
      ) 
    )
    | SAssign (s, e) ->
    (match e with
        
        |(_, SCall("read", vars)) -> let (img, (_,(new_matrix_map, new_image_map))) = read_body s vars builder matrix_map image_map in 
                                    ignore(L.build_store img (lookup s) builder); (L.const_int i32_t 0,(new_matrix_map, new_image_map))
          
        |_ -> let e' = fst (expr (builder, (matrix_map, image_map)) e) in ignore(L.build_store e' (lookup s) builder); (e', (matrix_map, image_map))
    )

    | SBinop ((A.Float,_ ) as e1, op, e2) ->
  	  let e1' = fst (expr (builder, (matrix_map, image_map)) e1)
  	  and e2' = fst (expr (builder , (matrix_map, image_map)) e2) in
  	  ((match op with 
  	    A.Add     -> L.build_fadd
  	  | A.Sub     -> L.build_fsub
  	  | A.Mult    -> L.build_fmul
  	  | A.Div     -> L.build_fdiv 
      | A.Mod     -> L.build_srem
  	  | A.Equal   -> L.build_fcmp L.Fcmp.Oeq
  	  | A.Neq     -> L.build_fcmp L.Fcmp.One
  	  | A.Less    -> L.build_fcmp L.Fcmp.Olt
  	  | A.Leq     -> L.build_fcmp L.Fcmp.Ole
  	  | A.Greater -> L.build_fcmp L.Fcmp.Ogt
  	  | A.Geq     -> L.build_fcmp L.Fcmp.Oge
  	  | A.And | A.Or ->
  	      raise (Failure "internal error: semant should have rejected and/or on float")
  	 ) e1' e2' "tmp" builder, (matrix_map, image_map))
    | SBinop (e1, op, e2) ->
      let e1' = fst (expr (builder, (matrix_map, image_map)) e1)
      and e2' = fst (expr (builder , (matrix_map, image_map)) e2) in
      ((
        match op with
          A.Add     -> L.build_add
        | A.Sub     -> L.build_sub
        | A.Mult    -> L.build_mul
        | A.Div     -> L.build_sdiv
        | A.And     -> L.build_and
        | A.Or      -> L.build_or
        | A.Equal   -> L.build_icmp L.Icmp.Eq
        | A.Neq     -> L.build_icmp L.Icmp.Ne
        | A.Less    -> L.build_icmp L.Icmp.Slt
        | A.Leq     -> L.build_icmp L.Icmp.Sle
        | A.Greater -> L.build_icmp L.Icmp.Sgt
        | A.Geq     -> L.build_icmp L.Icmp.Sge
      ) e1' e2' "tmp" builder, (matrix_map, image_map))

    | SUnop(op, ((t, _) as e)) ->
          let e' = fst (expr (builder, (matrix_map, image_map)) e) in
  	  ((match op with
  	    A.Neg when t = A.Float -> L.build_fneg 
  	  | A.Neg                  -> L.build_neg
      | A.Not                  -> L.build_not) e' "tmp" builder,(matrix_map, image_map))
    | SCall ("print", [e]) | SCall ("printb", [e]) ->
	  (L.build_call printf_func [| int_format_str ; (fst (expr (builder, (matrix_map, image_map)) e)) |]
	    "printf" builder, (matrix_map, image_map))
    | SCall ("printf", [e]) -> 
	  (L.build_call printf_func [| float_format_str ; (fst (expr (builder, (matrix_map, image_map)) e)) |]
	    "printf" builder, (matrix_map, image_map))
    (*| SCall ("prints", [e]) ->
	  L.build_call prints_func [| string_format_str ; (expr builder e) |]
	    "printf" builder*)
    | SCall ("prints", [e]) ->        
	  (L.build_call printf_func [| string_format_str ; (fst (expr (builder, (matrix_map, image_map)) e)) |] 
	    "printf" builder, (matrix_map, image_map))
    | SCall ("scale", e)     ->
          let args = e
          in let m = match args with mm::_ -> mm
          in let (row, col) = match m with (_, SId s) -> StringMap.find s matrix_map
          in let ratio = match args with _::rratio ->List.hd rratio

          in let stored_matrix = fst (expr (builder, (matrix_map, image_map)) m)
          in let rat = fst (expr (builder, (matrix_map, image_map)) ratio)  





          in let mat_ptr = L.build_gep stored_matrix [| L.const_int i32_t 0 |] "ptr_matrix" builder

          in let ptr_typ = ltype_of_typ A.Matrix
          in let func_def_scale = L.function_type i32_t [| ptr_typ; i32_t; i32_t; float_t |]
          in let func_decl_scale = L.declare_function "scale_c" func_def_scale the_module

          in ignore(L.build_call func_decl_scale [| mat_ptr;row; col;rat |] "" builder);
          (L.const_int i32_t 0, (matrix_map, image_map)) 
    |SCall("save", vars) -> 
            save_body vars builder matrix_map image_map
    |SCall("multiply", e)  ->
            let args = e
            in let m1 = List.nth args 0
            in let m2 = List.nth args 1
            in let m3 = List.nth args 2
            in let s1 = match m1 with (_, SId s) -> s
            in let s2 = match m2 with (_, SId s) -> s
            in let s3 = match m3 with (_, SId s) -> s
            in let (row1, col1) = StringMap.find s1 matrix_map
            in let (row2, col2) = StringMap.find s2 matrix_map
           (* in if (L.int64_of_const col1 != L.int64_of_const row2) 
                then raise(Failure("Matrix Multiplication must obey dimension restriction"))
                else ( *) 
           in let stored_mat1 = fst (expr (builder, (matrix_map, image_map)) m1)
           in let stored_mat2 = fst (expr (builder, (matrix_map, image_map)) m2)
           in let stored_mat3 = fst (expr (builder, (matrix_map, image_map)) m3)
           in let mat_ptr1 = L.build_gep stored_mat1 [| L.const_int i32_t 0 |] "ptr_matrix" builder
                    in let mat_ptr2 = L.build_gep stored_mat2 [| L.const_int i32_t 0 |] "ptr_matrix" builder
                    in let mat_ptr3 = L.build_gep stored_mat3 [| L.const_int i32_t 0 |] "ptr_matrix" builder
                    in let ptr_type = ltype_of_typ A.Matrix
                    in let func_def_multiply = L.function_type i32_t [| ptr_type; ptr_type; ptr_type; i32_t; i32_t |]
                    in let func_decl_multiply = L.declare_function "multiply_c" func_def_multiply the_module
                    in let new_matrix_map = StringMap.add s3 (row1, col2) matrix_map
                    in ignore(L.build_call func_decl_multiply [| mat_ptr1; mat_ptr2; mat_ptr3; row1; col1 |] "" builder);
                    (L.const_int i32_t 0, (new_matrix_map, image_map))
                
    | SCall ("rotate", e)       ->
            let args = e
            in let m = match args with mm::_ -> mm
            in let s = match m with (_, SId s) -> s
            in let (row, col) = StringMap.find s matrix_map
            in let direction = match args with _::rdirection -> List.hd rdirection
            in let stored_matrix = fst (expr (builder, (matrix_map, image_map)) m)
            in let dir = fst (expr (builder, (matrix_map, image_map)) direction)
            in let mat_ptr = L.build_gep stored_matrix [|L.const_int i32_t 0 |] "ptr_matrix" builder
            in let ptr_typ = ltype_of_typ A.Matrix
            in let func_def_rotate = L.function_type i32_t [| ptr_typ; i32_t; i32_t; i1_t |]
            in let func_decl_rotate = L.declare_function "rotate_c" func_def_rotate the_module
            in let new_matrix_map = StringMap.add s (col, row) matrix_map
            in ignore(L.build_call func_decl_rotate [| mat_ptr; row; col; dir|] "" builder);
            (L.const_int i32_t 0, (new_matrix_map, image_map))
    | SCall ("transpose", e)     ->
          let args = e
          in let m = match args with mm::_ -> mm
          in let s = match m with (_, SId s) -> s
          in let (row, col) = StringMap.find s matrix_map


          in let stored_matrix = fst (expr (builder, (matrix_map, image_map)) m)






          in let mat_ptr = L.build_gep stored_matrix [| L.const_int i32_t 0 |] "ptr_matrix" builder

          in let ptr_typ = ltype_of_typ A.Matrix
          in let func_def_transpose = L.function_type i32_t [| ptr_typ ; i32_t; i32_t; |]
          in let func_decl_transpose = L.declare_function "transpose_c" func_def_transpose the_module
          in let new_matrix_map = StringMap.add s (col, row) matrix_map
          in ignore(L.build_call func_decl_transpose [| mat_ptr; row; col |] "" builder);
          (L.const_int i32_t 0, (new_matrix_map, image_map)) 
    | SCall (f, args) ->
    let (fdef, fdecl) = StringMap.find f function_decls in
    let llargs = List.rev (List.map fst (List.map (expr (builder, (matrix_map, image_map))) (List.rev args))) in
    let result = (match fdecl.styp with 
                    A.Void -> ""
                  | _ -> f ^ "_result") in
       (L.build_call fdef (Array.of_list llargs) result builder, (matrix_map, image_map))
    in
    (* LLVM insists each basic block end with exactly one "terminator" 
       instruction that transfers control.  This function runs "instr builder"
       if the current block does not already have a terminator.  Used,
       e.g., to handle the "fall off the end of the function" case. *)
    let add_terminal builder_and_maps instr =
      match L.block_terminator (L.insertion_block (fst builder_and_maps)) with
	      Some _ -> ()
      | None -> ignore (instr (fst builder_and_maps)) in
	
    (* Build the code for the given statement; return the builder for
       the statement's successor (i.e., the next instruction will be built
       after the one generated by this call) *)

    let rec stmt (builder, (matrix_map, image_map)) = function
	      SBlock sl -> List.fold_left stmt (builder, (matrix_map, image_map)) sl
      | STypeAsn (type_of_id, id) -> (builder, (matrix_map, image_map))
      | SDeclAsn ((type_of_id, id), exprs) ->
      (match exprs with
        
        |(_, SCall("read", vars)) -> let (img, (new_builder,(new_matrix_map, new_image_map))) = read_body id vars builder matrix_map image_map in 
                                    ignore(L.build_store img (lookup id) new_builder); (new_builder,(new_matrix_map, new_image_map))
        
        |_ -> let (e', _) = expr (builder, (matrix_map, image_map)) exprs in
                        ignore(L.build_store e' (lookup id) builder); (builder, (matrix_map, image_map))
      )
      | SExpr e -> let (_, (new_matrix_map, _)) = expr (builder, (matrix_map, image_map)) e in (builder, (new_matrix_map, image_map))
      | SReturn e -> ignore(match fdecl.styp with
                            (* Special "return nothing" instr *)
                            A.Void -> L.build_ret_void builder 
                            (* Build return statement *)
                          | _ -> L.build_ret (fst (expr (builder, (matrix_map, image_map)) e)) builder );
                     (builder, (matrix_map, image_map))
      | SIf (predicate, then_stmt, else_stmt) ->
         let bool_val = fst (expr (builder, (matrix_map, image_map)) predicate) in
	       let merge_bb = L.append_block context "merge" the_function in
         let build_br_merge = L.build_br merge_bb in (* partial function *)

      	 let then_bb = L.append_block context "then" the_function in
      	 add_terminal (stmt (L.builder_at_end context then_bb, (matrix_map, image_map)) then_stmt)
      	   build_br_merge;

      	 let else_bb = L.append_block context "else" the_function in
      	 add_terminal (stmt (L.builder_at_end context else_bb, (matrix_map, image_map)) else_stmt)
      	   build_br_merge;

      	 ignore(L.build_cond_br bool_val then_bb else_bb builder);
      	 (L.builder_at_end context merge_bb, (matrix_map, image_map))

      | SMatDeclAsn(ty, s, e1, e2, exprs) ->
      (
        let e1' = fst (expr (builder, (matrix_map, image_map)) e1) in
        let e2' = fst (expr (builder, (matrix_map, image_map)) e2) in
        let size = (e1', e2') in
        let e' = fst (expr (builder, (matrix_map, image_map)) exprs) in
        let new_matrix_map = StringMap.add s size matrix_map in
        ignore(L.build_store e' (lookup s) builder); (builder, (new_matrix_map, image_map))
      )

      | SMatDecl(ty, s, e1, e2) ->
      (
        let e1' = fst (expr (builder, (matrix_map, image_map)) e1) in
        let e2' = fst (expr (builder, (matrix_map, image_map)) e2) in
        let size = (e1', e2') in
        let new_matrix_map = StringMap.add s size matrix_map in
        (builder, (new_matrix_map, image_map))
      )



      | SWhile (predicate, body) ->
    	  let pred_bb = L.append_block context "while" the_function in
    	  ignore(L.build_br pred_bb builder);

    	  let body_bb = L.append_block context "while_body" the_function in
    	  add_terminal (stmt (L.builder_at_end context body_bb, (matrix_map, image_map)) body)
    	    (L.build_br pred_bb);

    	  let pred_builder = L.builder_at_end context pred_bb in
    	  let bool_val = fst (expr (pred_builder, (matrix_map, image_map)) predicate) in

    	  let merge_bb = L.append_block context "merge" the_function in
    	  ignore(L.build_cond_br bool_val body_bb merge_bb pred_builder);
    	  (L.builder_at_end context merge_bb, (matrix_map, image_map))

      (* Implement for loops as while loops *)
      | SFor (e1, e2, e3, body) -> stmt (builder, (matrix_map, image_map))
	    ( SBlock [SExpr e1 ; SWhile (e2, SBlock [body ; SExpr e3]) ] )
      in

      (* Build the code for each statement in the function *)
      let builder_and_maps = stmt (builder, (StringMap.empty, StringMap.empty)) (SBlock fdecl.sbody) in

      (* Add a return if the last block falls off the end *)
      add_terminal builder_and_maps (match fdecl.styp with
          A.Void -> L.build_ret_void
        | A.Float -> L.build_ret (L.const_float float_t 0.0)
        | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
    in

  List.iter build_function_body functions;
  the_module


(*       
      let rec recompute_in = function
        | [] -> []
        | head :: tail -> let res = expr builder head in res :: recompute_in tail 
      in 
      let rec recompute_out = function
        | [[]] -> let x = L.const_array float_t (Array.of_list([])) in [x]
        | head::tail -> 
        (
          (* inner *)
          let tmp = Array.of_list(recompute_in head) in 
          let res = L.const_array float_t tmp in
          res :: recompute_out tail
        )
        | [] -> []
        (* | _ -> raise(Failure("invalid matlit")) *)
      in  *)
(*       let matrix_o = L.build_malloc matrix_t "res" builder in
      (L.const_array (array_t col) (Array.of_list(recompute_out el)))
      matrix_o *)
