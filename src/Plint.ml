open Core.Std

(* === Check Source File === *)

type error = {
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
        let step0 = context in
        let step1 = eval step0 func in
        let step2 = BatList.fold_left eval step1 args in
        let step3 = BatList.fold_left eval step2 (keyword_values keywords) in
        let step4 = eval_option step3 starargs in
        let step5 = eval_option step4 kwargs in
        step5
      
      | Num { n = n } ->
        context
      
      | Str { s = s } ->
        context
      
      | Name { id = id; ctx = ctx; location = location } ->
        (match ctx with
          | Load
          | AugLoad ->
            if BatSet.mem id context.names then
              context
            else
              let new_error = {
                line = location.lineno;
                exn = "NameError: name '" ^ id ^ "' is not defined"
              } in
              { context with errors = new_error :: context.errors }
          
          | Store
          | AugStore
          | Param -> 
            context
          
          | Del ->
            context
        )
    and
  
  (eval_option : exec_context -> PyAst.expr option -> exec_context) context expr_option =
    match expr_option with
      | Some expr ->
        eval context expr
      
      | None ->
        context and
  
  (eval_list : exec_context -> PyAst.expr list -> exec_context) context expr_list =
    BatList.fold_left eval context expr_list

let rec
  (eval_assign : exec_context -> PyAst.expr -> exec_context) context expr =
    let open PyAst in
    match expr with
      (* TODO: Recognize remaining exprs valid in assignment context. 6 total. *)
      | Name { id = id; ctx = ctx; location = location } ->
        (match ctx with
          | Load
          | AugLoad ->
            let new_error = {
              line = location.lineno;
              exn = "SystemError: Name in Assign stmt marked as ctx=Load|AssignLoad"
            } in
            { context with errors = new_error :: context.errors }
          
          | Store
          | AugStore
          | Param -> 
            { context with names = BatSet.add id context.names }
          
          | Del ->
            { context with names = BatSet.remove id context.names }
        )
      
      (* Ignore all expr types that aren't valid in an assign context *)
      | Call _
      | Num _
      | Str _ ->
        context
    and
  
  (eval_assign_list : exec_context -> PyAst.expr list -> exec_context) context targets =
    BatList.fold_left eval_assign context targets

let (exec : exec_context -> PyAst.stmt -> exec_context) context stmt = 
  let open PyAst in
  match stmt with
    | Delete { targets = targets } ->
      let step0 = context in
      let step1 = eval_list step0 targets in
      let step2 = eval_assign_list step1 targets in
      step2
    
    | Assign { targets = targets; value = value } ->
      let step0 = context in
      let step1 = eval_list step0 targets in
      let step2 = eval step1 value in
      let step3 = eval_assign_list step2 targets in
      step3
    
    | AugAssign { target = target; op = op; value = value } ->
      let step0 = context in
      let step1 = eval step0 target in
      let step2 = eval step1 value in
      let step3 = eval_assign step2 target in
      step3
    
    | Expr { value = value } ->
      eval context value

(** Checks the specified Python source file for errors. *)
let (check : string -> error list) py_filepath =
  match PyAst.parse_ast_of_file py_filepath with
    | None ->
      (* TODO: Report actual line number of the syntax error *)
      [{ line = 1; exn = "SystemError: unable to parse intermediate AST" }]
    
    | Some ast ->
      let (stmts : PyAst.stmt list) = ast in
      
      let builtins = ["print"] in
      let initial_context = { names = BatSet.of_list builtins; errors = [] } in
      let final_context = BatList.fold_left exec initial_context stmts in
      
      let { errors = final_errors } = final_context in
      BatList.rev final_errors  (* order errors from first to last *)

(** Formats a human-readable description of the specified error. *)
let (descripton_of_error : error -> string) error =
  "Traceback (most recent call last):\n" ^
  "  Line " ^ (string_of_int error.line) ^ ", in <module>\n" ^
  "    prnt('Hello')\n" ^
  error.exn

(* === Utility === *)

(** Formats a list of errors as a printable string. *)
let (string_of_error_list : error list -> string) error_list =
  Sexp.to_string (sexp_of_list sexp_of_error error_list)
