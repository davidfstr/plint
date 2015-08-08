open Core.Std

(* === Check Source File === *)

type error = {
  file : string;
  line : int;
  exn : string
} with sexp

type exec_context = { names : string BatSet.t; errors : error list }

let rec 
  (eval : exec_context -> PyAst.expr -> exec_context) context expr = 
    let open PyAst in
    match expr with
      | Call {
        func = func;
        args = args;
        keywords = keywords;
        starargs = starargs;
        kwargs = kwargs
      } ->
        let step1 = eval context func in
        let step2 = BatList.fold_left eval step1 args in
        let step3 = BatList.fold_left eval step2 (keyword_values keywords) in
        let step4 = eval_option step3 starargs in
        let step5 = eval_option step4 kwargs in
        step5
      
      | Str { s = s } ->
        context
      
      | Name { id = id; ctx = ctx } ->
        (* TODO: Take the ctx into account *)
        if BatSet.mem id context.names then
          context
        else
          let new_error = {
            (* TODO: Generate real file name *)
            file = "src/test_data/bad_1_prnt.py";
            (* TODO: Generate real line number from ast *)
            line = 1;
            exn = "NameError: name '" ^ id ^ "' is not defined"
          } in
          { names = context.names; errors = new_error :: context.errors }
    and
  
  (eval_option : exec_context -> PyAst.expr option -> exec_context) context expr_option =
    match expr_option with
      | Some expr ->
        eval context expr
      
      | None ->
        context

let (exec : exec_context -> PyAst.stmt -> exec_context) context stmt = 
  let open PyAst in
  match stmt with
    | Expr { value = expr } ->
      eval context expr

(* Checks the specified Python source file for errors. *)
let (check : string -> error list) py_filepath =
  match PyAst.parse_ast_of_file py_filepath with
    | None ->
      (* TODO: Report actual line number of the syntax error *)
      [{ file = py_filepath; line = 1; exn = "SyntaxError: invalid syntax" }]
    
    | Some ast ->
      let (stmts : PyAst.stmt list) = ast in
      
      let builtins = ["print"] in
      let initial_context = { names = BatSet.of_list builtins; errors = [] } in
      let final_context = BatList.fold_left exec initial_context stmts in
      
      let { errors = final_errors } = final_context in
      final_errors

(* Formats a human-readable description of the specified error. *)
let (descripton_of_error : error -> string) error =
  (* TODO: Fix to include the correct excerpt line *)
  "Traceback (most recent call last):\n" ^
  "  File \"" ^ error.file ^ "\", line " ^ (string_of_int error.line) ^ ", in <module>\n" ^
  "    prnt('Hello')\n" ^
  error.exn

(* === Utility === *)

(** Formats a list of errors as a printable string. *)
let (string_of_error_list : error list -> string) error_list =
  Sexp.to_string (sexp_of_list sexp_of_error error_list)