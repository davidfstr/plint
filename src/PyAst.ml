open Core.Std
open Option.Monad_infix (* for `>>=` *)
open Yojson.Basic.Util  (* for `member` *)

(* === AST Types === *)
type
  ast =
    | Module of ast_Module
    and
  ast_Module = {
    body : stmt list;
  } and
  
  stmt =
    | Delete of stmt_Delete
    | Assign of stmt_Assign
    | AugAssign of stmt_AugAssign
    | Expr of stmt_Expr
    and
  stmt_Delete = {
    targets : expr list;
    location : location;
  } and
  stmt_Assign = {
    targets : expr list;
    value : expr;
    location : location;
  } and
  stmt_AugAssign = {
    target : expr;
    op : operator;
    value : expr;
    location : location;
  } and
  stmt_Expr = {
    value : expr;
    location : location;
  } and
  
  expr =
    | Call of expr_Call
    | Num of expr_Num
    | Str of expr_Str
    | Name of expr_Name
    and
  expr_Call = {
    func : expr;
    args : expr list;
    keywords : keyword list;
    starargs : expr option;
    kwargs : expr option;
    location : location;
  } and
  expr_Num = {
    n : int;
    location : location;
  } and
  expr_Str = {
    s : string;
    location : location;
  } and
  expr_Name = {
    id : identifier;
    ctx : expr_context;
    location : location;
  } and
  
  expr_context =
    | Load
    | Store
    | Del
    | AugLoad
    | AugStore
    | Param
    and
  
  operator =
    | Add
    | Sub
    | Mult
    | Div
    | Mod
    | Pow
    | LShift
    | RShift
    | BitOr
    | BitXor
    | BitAnd
    | FloorDiv
    and
  
  keyword = {
    arg : identifier;
    value : expr;
  } and
  
  location = { lineno : int; col_offset : int } and
  
  (* --- Builtins --- *)
  identifier = string

  with sexp

(* === Parse AST from JSON === *)
let rec
  parse_ast json =
    match json with
      | `List [`String "Module"; members_json] ->
        let body_json = members_json |> member "body" in
        
        parse_stmt_list body_json >>= fun body ->
        
        Some (Module {
          body = body;
        })
      
      | `List [`String unknown_type; _; _] ->
        let () = printf "*** PyAst: unrecognized kind of ast: %s\n" unknown_type in
        None
      
      | _ ->
        None and
  
  parse_ast_list json =
    match json with
      | `List item_jsons ->
        Option.all (List.map item_jsons parse_ast) >>= fun items ->
        Some items
      
      | _ ->
        None and
  
  parse_ast_option json =
    match json with
      | `Null ->
        Some None
      
      | _ ->
        parse_ast json >>= fun ast ->
        Some (Some ast) and
  
  parse_stmt json =
    match json with
      | `List [`String "Delete"; members_json; attributes_json] ->
        let targets_json = members_json |> member "targets" in
        
        parse_expr_list targets_json >>= fun targets ->
        
        parse_location attributes_json >>= fun location ->
        
        Some (Delete {
          targets = targets;
          location = location;
        })
      
      | `List [`String "Assign"; members_json; attributes_json] ->
        let targets_json = members_json |> member "targets" in
        let value_json = members_json |> member "value" in
        
        parse_expr_list targets_json >>= fun targets ->
        parse_expr value_json >>= fun value ->
        
        parse_location attributes_json >>= fun location ->
        
        Some (Assign {
          targets = targets;
          value = value;
          location = location;
        })
      
      | `List [`String "AugAssign"; members_json; attributes_json] ->
        let target_json = members_json |> member "target" in
        let op_json = members_json |> member "op" in
        let value_json = members_json |> member "value" in
        
        parse_expr target_json >>= fun target ->
        parse_operator op_json >>= fun op ->
        parse_expr value_json >>= fun value ->
        
        parse_location attributes_json >>= fun location ->
        
        Some (AugAssign {
          target = target;
          op = op;
          value = value;
          location = location;
        })
      
      | `List [`String "Expr"; members_json; attributes_json] ->
        let value_json = members_json |> member "value" in
        
        parse_expr value_json >>= fun value ->
        
        parse_location attributes_json >>= fun location ->
        
        Some (Expr {
          value = value;
          location = location;
        })
      
      | `List [`String unknown_type; _; _] ->
        let () = printf "*** PyAst: unrecognized kind of stmt: %s\n" unknown_type in
        None
      
      | _ ->
        None and
  
  parse_stmt_list json =
    match json with
      | `List item_jsons ->
        Option.all (List.map item_jsons parse_stmt) >>= fun items ->
        Some items
      
      | _ ->
        None and
  
  parse_stmt_option json =
    match json with
      | `Null ->
        Some None
      
      | _ ->
        parse_stmt json >>= fun stmt ->
        Some (Some stmt) and
  
  parse_expr json =
    match json with
      | `List [`String "Call"; members_json; attributes_json] ->
        let func_json = members_json |> member "func" in
        let args_json = members_json |> member "args" in
        let keywords_json = members_json |> member "keywords" in
        let starargs_json = members_json |> member "starargs" in
        let kwargs_json = members_json |> member "kwargs" in
        
        parse_expr func_json >>= fun func ->
        parse_expr_list args_json >>= fun args ->
        parse_keyword_list keywords_json >>= fun keywords ->
        parse_expr_option starargs_json >>= fun starargs ->
        parse_expr_option kwargs_json >>= fun kwargs ->
        
        parse_location attributes_json >>= fun location ->
        
        Some (Call {
          func = func;
          args = args;
          keywords = keywords;
          starargs = starargs;
          kwargs = kwargs;
          location = location;
        })
      
      | `List [`String "Num"; members_json; attributes_json] ->
        let n_json = members_json |> member "n" in
        
        parse_int n_json >>= fun n ->
        
        parse_location attributes_json >>= fun location ->
        
        Some (Num {
          n = n;
          location = location;
        })
      
      | `List [`String "Str"; members_json; attributes_json] ->
        let s_json = members_json |> member "s" in
        
        parse_string s_json >>= fun s ->
        
        parse_location attributes_json >>= fun location ->
        
        Some (Str {
          s = s;
          location = location;
        })
      
      | `List [`String "Name"; members_json; attributes_json] ->
        let id_json = members_json |> member "id" in
        let ctx_json = members_json |> member "ctx" in
        
        parse_identifier id_json >>= fun id ->
        parse_expr_context ctx_json >>= fun ctx ->
        
        parse_location attributes_json >>= fun location ->
        
        Some (Name {
          id = id;
          ctx = ctx;
          location = location;
        })
      
      | `List [`String unknown_type; _; _] ->
        let () = printf "*** PyAst: unrecognized kind of expr: %s\n" unknown_type in
        None
      
      | _ ->
        None and
  
  parse_expr_list json =
    match json with
      | `List item_jsons ->
        Option.all (List.map item_jsons parse_expr) >>= fun items ->
        Some items
      
      | _ ->
        None and
  
  parse_expr_option json =
    match json with
      | `Null ->
        Some None
      
      | _ ->
        parse_expr json >>= fun expr ->
        Some (Some expr) and
  
  parse_expr_context json =
    match json with
      | `List [`String "Load"; members_json] ->
        Some Load
      
      | `List [`String "Store"; members_json] ->
        Some Store
      
      | `List [`String "Del"; members_json] ->
        Some Del
      
      | `List [`String "AugLoad"; members_json] ->
        Some AugLoad
      
      | `List [`String "AugStore"; members_json] ->
        Some AugStore
      
      | `List [`String "Param"; members_json] ->
        Some Param
      
      | `List [`String unknown_type; _; _] ->
        let () = printf "*** PyAst: unrecognized kind of expr_context: %s\n" unknown_type in
        None
      
      | _ ->
        None and
  
  parse_expr_context_list json =
    match json with
      | `List item_jsons ->
        Option.all (List.map item_jsons parse_expr_context) >>= fun items ->
        Some items
      
      | _ ->
        None and
  
  parse_expr_context_option json =
    match json with
      | `Null ->
        Some None
      
      | _ ->
        parse_expr_context json >>= fun expr_context ->
        Some (Some expr_context) and
  
  parse_operator json =
    match json with
      | `List [`String "Add"; members_json] ->
        Some Add
      
      | `List [`String "Sub"; members_json] ->
        Some Sub
      
      | `List [`String "Mult"; members_json] ->
        Some Mult
      
      | `List [`String "Div"; members_json] ->
        Some Div
      
      | `List [`String "Mod"; members_json] ->
        Some Mod
      
      | `List [`String "Pow"; members_json] ->
        Some Pow
      
      | `List [`String "LShift"; members_json] ->
        Some LShift
      
      | `List [`String "RShift"; members_json] ->
        Some RShift
      
      | `List [`String "BitOr"; members_json] ->
        Some BitOr
      
      | `List [`String "BitXor"; members_json] ->
        Some BitXor
      
      | `List [`String "BitAnd"; members_json] ->
        Some BitAnd
      
      | `List [`String "FloorDiv"; members_json] ->
        Some FloorDiv
      
      | `List [`String unknown_type; _; _] ->
        let () = printf "*** PyAst: unrecognized kind of operator: %s\n" unknown_type in
        None
      
      | _ ->
        None and
  
  parse_operator_list json =
    match json with
      | `List item_jsons ->
        Option.all (List.map item_jsons parse_operator) >>= fun items ->
        Some items
      
      | _ ->
        None and
  
  parse_operator_option json =
    match json with
      | `Null ->
        Some None
      
      | _ ->
        parse_operator json >>= fun operator ->
        Some (Some operator) and
  
  parse_keyword json =
    match json with
      | `List [`String "keyword"; members_json] ->
        let arg_json = members_json |> member "arg" in
        let value_json = members_json |> member "value" in
        
        parse_identifier arg_json >>= fun arg ->
        parse_expr value_json >>= fun value ->
        
        Some ({
          arg = arg;
          value = value;
        })
      
      | `List [`String unknown_type; _; _] ->
        let () = printf "*** PyAst: unrecognized kind of keyword: %s\n" unknown_type in
        None
      
      | _ ->
        None and
  
  parse_keyword_list json =
    match json with
      | `List item_jsons ->
        Option.all (List.map item_jsons parse_keyword) >>= fun items ->
        Some items
      
      | _ ->
        None and
  
  parse_keyword_option json =
    match json with
      | `Null ->
        Some None
      
      | _ ->
        parse_keyword json >>= fun keyword ->
        Some (Some keyword) and
  
  (* --- Builtins --- *)
  
  parse_identifier json =
    match json with
      | `String string ->
        Some string
      
      | _ ->
        None and
  
  parse_string json =
    match json with
      | `String string ->
        Some string
      
      | _ ->
        None and
  
  parse_int json =
    match json with
      | `Int n ->
        Some n
      
      | _ ->
        None and
  
  parse_location attributes_json =
    match attributes_json with
      | `Assoc [("lineno", `Int lineno); ("col_offset", `Int col_offset)] ->
        Some { lineno = lineno; col_offset = col_offset }
      
      | _ ->
        None
