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
        let (keyword_values : keyword list -> expr list) keywords =
          BatList.map (fun (k : keyword) -> k.value) keywords in
        
        let step0 = context in
        let step1 = eval step0 func in
        let step2 = BatList.fold_left eval step1 args in
        let step3 = BatList.fold_left eval step2 (keyword_values keywords) in
        let step4 = eval_option step3 starargs in
        let step5 = eval_option step4 kwargs in
        step5
      
      | Num { n = _ }
      | Str { s = _ }
      | NameConstant { value = _ } ->
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
      | Str _
      | NameConstant _ ->
        context
    and
  
  (eval_assign_list : exec_context -> PyAst.expr list -> exec_context) context targets =
    BatList.fold_left eval_assign context targets

let (string_of_exec_context : exec_context -> string) context =
  Sexp.to_string (sexp_of_list sexp_of_string (BatSet.to_list context.names))

let rec
  (exec : exec_context -> PyAst.stmt -> exec_context) context stmt = 
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
      
      | While { test = test; body = body; orelse = orelse; location = location } ->
        let condition_join_point = ref context in
        let nullable_exit_join_point = ref None in
        let join_points_were_changed = ref true in
        
        let (merge : exec_context ref -> exec_context -> unit) join_point future_context =
          let merged = {
            names = BatSet.intersect (!join_point).names future_context.names;
            errors = future_context.errors
          } in
          (if not (BatSet.equal (!join_point).names merged.names) then
            join_points_were_changed := true
          else
            ()
          ) ;
          join_point := merged in
        
        let (merge_nullable : ((exec_context ref) option) ref -> exec_context -> unit) nullable_join_point future_context =
          (match !nullable_join_point with
            | None ->
              nullable_join_point := Some { contents = future_context } ;
              join_points_were_changed := true
            
            | Some join_point ->
              merge join_point future_context
          ) in
        
        (* Execute loop condition and body until final environment deduced *)
        let k_max_distinct_loop_iterations = 2 in
        let i = ref 0 in
        while (!join_points_were_changed) do
          (if (!i >= k_max_distinct_loop_iterations) then
            (* Append error to final result *)
            let new_error = {
              line = location.lineno;
              exn = sprintf "SystemError: could not compute final type environment for loop within %d iterations" k_max_distinct_loop_iterations
            } in
            (match !nullable_exit_join_point with
              | Some exit_join_point ->
                exit_join_point := {
                  !exit_join_point with
                  errors = new_error :: (!exit_join_point).errors
                }
              
              | None ->
                assert false
            ) ;
            
            (* Force exit of loop and return of final result *)
            join_points_were_changed := false
          else
            join_points_were_changed := false ;
            
            let step0 = !condition_join_point in
            let step1 = eval step0 test in
            merge_nullable nullable_exit_join_point step1 ;
            
            let step2 = exec_list step1 body in
            merge condition_join_point step2 ;
            
            i := !i + 1
          )
        done ;
        
        let exit_context = (match !nullable_exit_join_point with
          | Some exit_join_point ->
            !exit_join_point
          
          | None ->
            assert false
        ) in
        
        (* Execute else-block if present *)
        (* NOTE: Shouldn't execute this if loop exited due to a break statement *)
        exec_list exit_context orelse
      
      | If { test = test; body = body; orelse = orelse } ->
        let step0 = context in
        let step1 = eval step0 test in
        let step1_noerr = { step1 with errors = [] } in
        let step2a = exec_list step1_noerr body in
        let step2b = exec_list step1_noerr orelse in
        let step3 = {
          names = BatSet.intersect step2a.names step2b.names;
          (* Concat in reverse order since error list is reversed *)
          errors = step2b.errors @ step2a.errors @ step1.errors
        } in
        step3
      
      | Expr { value = value } ->
        eval context value and
  
  (exec_list : exec_context -> PyAst.stmt list -> exec_context) context stmt_list =
    BatList.fold_left exec context stmt_list

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
      let final_context = exec_list initial_context stmts in
      
      let { errors = errors0 } = final_context in
      (* Order errors from first to last *)
      let errors1 = BatList.rev errors0 in
      (* Deduplicate errors *)
      (* PERF: Takes O(n^2) time but could be optimized to O(n) time *)
      let errors2 = BatList.unique errors1 in
      errors2

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
