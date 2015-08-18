open Core.Std

(* === Execution Context === *)

type error = {
  line : int;
  exn : string
} with sexp

type typ =
  | FuncRef of PyAst.stmt_FunctionDef
  | Unknown
  with sexp

type exec_context = {
  names : (string, typ) BatMap.t;
  errors : error list
}

let (is_defined : exec_context -> string -> bool) context name =
  BatMap.mem name context.names

let (define : exec_context -> string -> typ -> exec_context) context name typ =
  { context with names = BatMap.add name typ context.names }

let (undefine : exec_context -> string -> exec_context) context name =
  { context with names = BatMap.remove name context.names }

let (intersect_names :
    (string, typ) BatMap.t -> 
    (string, typ) BatMap.t ->
    (string, typ) BatMap.t)
    names1 names2 =
  
  let (join : typ -> typ -> typ) typ1 typ2 = 
    match (typ1, typ2) with
      | (Unknown, _)
      | (_, Unknown) ->
        Unknown
      
      | (FuncRef f, FuncRef g) ->
        if f = g then
          FuncRef f
        else
          (* TODO: Emit warning about unable to perserve type information *)
          Unknown
    in
  BatMap.intersect join names1 names2

let (equal_names :
    (string, typ) BatMap.t -> 
    (string, typ) BatMap.t ->
    bool)
    names1 names2 =
  
  BatMap.equal (=) names1 names2

let (sexp_of_string_typ : (string * typ) -> Sexp.t) binding =
  let open Core.Std.Sexp in
  let (k, v) = binding in
  List [Atom k; sexp_of_typ v]

let (sexp_of_exec_context : exec_context -> Sexp.t) context =
  sexp_of_list sexp_of_string_typ (BatMap.bindings context.names)

let (string_of_exec_context : exec_context -> string) context =
  Sexp.to_string (sexp_of_exec_context context)

(* === Check Source File === *)

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
        let step2 = eval_list step1 args in
        let step3 = eval_list step2 (keyword_values keywords) in
        let step4 = eval_option step3 starargs in
        let step5 = eval_option step4 kwargs in
        (* FIXME: Perform call *)
        step5
      
      | Num { n = _ }
      | Str { s = _ }
      | NameConstant { value = _ } ->
        context
      
      | Name { id = id; ctx = ctx; location = location } ->
        (match ctx with
          | Load
          | AugLoad ->
            if is_defined context id then
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
  (eval_assign : 
      exec_context -> PyAst.expr -> typ -> exec_context)
      context lvalue rvalue =
    let open PyAst in
    match lvalue with
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
            define context id rvalue
          
          | Del ->
            undefine context id
        )
      
      (* Ignore all expr types that aren't valid in an assign context *)
      | Call _
      | Num _
      | Str _
      | NameConstant _ ->
        context
    and
  
  (eval_assign_list :
      exec_context -> PyAst.expr list -> typ -> exec_context)
      context targets rvalue =
    let f = (fun context target -> eval_assign context target rvalue) in
    BatList.fold_left f context targets

let rec
  (exec : exec_context -> PyAst.stmt -> exec_context) context stmt = 
    let open PyAst in
    match stmt with
      | FunctionDef f ->
        (match f with
          | { name = name; args = args; body = body;
              decorator_list = decorator_list; returns = returns;
              location = location } ->
            let (name_expr : expr) = Name {
              id = name;
              ctx = Store;
              location = location
            } in
            eval_assign context name_expr (FuncRef f)
        )
      
      | Delete { targets = targets } ->
        let step0 = context in
        let step1 = eval_list step0 targets in
        let step2 = eval_assign_list step1 targets Unknown in
        step2
      
      | Assign { targets = targets; value = value } ->
        let step0 = context in
        let step1 = eval_list step0 targets in
        let step2 = eval step1 value in
        (* TODO: Propagate type of value through assignment *)
        let step3 = eval_assign_list step2 targets Unknown in
        step3
      
      | AugAssign { target = target; op = op; value = value } ->
        let step0 = context in
        let step1 = eval step0 target in
        let step2 = eval step1 value in
        (* TODO: Propagate type of value through assignment *)
        let step3 = eval_assign step2 target Unknown in
        step3
      
      | While { test = test; body = body; orelse = orelse; location = location } ->
        let condition_join_point = ref context in
        let nullable_exit_join_point = ref None in
        let join_points_were_changed = ref true in
        
        let (merge : exec_context ref -> exec_context -> unit) join_point future_context =
          let merged = {
            names = intersect_names (!join_point).names future_context.names;
            errors = future_context.errors
          } in
          (if not (equal_names (!join_point).names merged.names) then
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
          names = intersect_names step2a.names step2b.names;
          (* Concat in reverse order since error list is reversed *)
          errors = step2b.errors @ step2a.errors @ step1.errors
        } in
        step3
      
      | Expr { value = value } ->
        eval context value
      
      | Pass { location = location } ->
        context
      
    and
  
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
      
      let builtins = [("print", Unknown)] in
      let initial_context = {
        names = BatMap.of_enum (BatList.enum builtins);
        errors = []
      } in
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
