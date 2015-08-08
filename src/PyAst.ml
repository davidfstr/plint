open Core.Std
open Option.Monad_infix (* for `>>=` *)
open Yojson.Basic.Util  (* for `member` *)

(* === AST Types === *)
type
  (** 
   * Root node of a Python AST.
   * 
   * Corresponds to an ast.Module from Python's abstract grammer, documented at:
   * https://docs.python.org/3.4/library/ast.html
   *)
  ast = stmt list and

  (* TODO: Recognize remaining types of stmt. 19 total. *)
  (* All stmts additionally track their location. *)
  stmt =
    | Expr of stmt_Expr and
  stmt_Expr = { value : expr; location : location } and

  (* TODO: Recognize remaining types of expr. 26 total. *)
  (* All exprs additionally track their location. *)
  expr =
    | Call of expr_Call
    | Str of expr_Str
    | Name of expr_Name and
  expr_Call = {
    func : expr;
    args : expr list;
    keywords : keyword list;
    starargs : expr option;
    kwargs : expr option;
    location : location
  } and
  expr_Str = { s : string; location : location } and 
  expr_Name = { id : identifier; ctx : expr_context; location : location } and
  
  (** Location of a stmt or expr in the original source file. *)
  location = { lineno : int; col_offset : int } and
  
  expr_context = Load | Store | Del | AugLoad | AugStore | Param and

  keyword = { arg : identifier; value : expr } and
  
  (* 
   * ASDL's six builtin types are:
   * identifier, int, string, bytes, object, singleton
   *)
  identifier = string

  with sexp


let (keyword_values : keyword list -> expr list) keywords =
  BatList.map (fun (k : keyword) -> k.value) keywords


(* === Parse AST from JSON === *)
let rec
  parse_ast json =
    match json with
      | `List [`String "Module"; members_json] ->
        let body_json     = members_json |> member "body" in
        
        parse_stmt_list    body_json       >>= fun body ->
        
        Some body
      
      | _ ->
        None and
  
  parse_stmt json =
    match json with
      | `List [`String "Expr"; members_json; attributes_json] ->
        let value_json    = members_json |> member "value" in
        
        parse_expr          value_json      >>= fun value ->
        
        parse_location      attributes_json >>= fun location ->
        
        Some (Expr { value = value; location = location })
      
      | `List [`String unknown_type; _] ->
        let () = printf "*** PyAst: unrecognized kind of stmt: %s\n" unknown_type in
        None
      
      | _ ->
        None and
  
  (* TODO: Combine with other parse_*_list functions *)
  parse_stmt_list json =
    match json with
      | `List item_jsons ->
        Option.all (List.map item_jsons parse_stmt) >>= fun items ->
        Some items
      
      | _ ->
        None and
  
  parse_expr json =
    match json with
      | `List [`String "Call"; members_json; attributes_json] ->
        let func_json     = members_json |> member "func" in
        let args_json     = members_json |> member "args" in
        let keywords_json = members_json |> member "keywords" in
        let starargs_json = members_json |> member "starargs" in
        let kwargs_json   = members_json |> member "kwargs" in
        
        parse_expr          func_json       >>= fun func ->
        parse_expr_list     args_json       >>= fun args ->
        parse_keyword_list  keywords_json   >>= fun keywords ->
        parse_expr_option   starargs_json   >>= fun starargs ->
        parse_expr_option   kwargs_json     >>= fun kwargs ->
        
        parse_location      attributes_json >>= fun location ->
        
        Some (Call {
          func = func;
          args = args;
          keywords = keywords;
          starargs = starargs;
          kwargs = kwargs;
          location = location
        })
      
      | `List [`String "Str"; members_json; attributes_json] ->
        let s_json        = members_json |> member "s" in
        
        parse_string        s_json          >>= fun s ->
        
        parse_location      attributes_json >>= fun location ->
        
        Some (Str { s = s; location = location })
      
      | `List [`String "Name"; members_json; attributes_json] ->
        let id_json       = members_json |> member "id" in
        let ctx_json      = members_json |> member "ctx" in
        
        parse_identifier    id_json         >>= fun id ->
        parse_expr_context  ctx_json        >>= fun ctx ->
        
        parse_location      attributes_json >>= fun location ->
        
        Some (Name { id = id; ctx = ctx; location = location })
      
      | `List [`String unknown_type; _] ->
        let () = printf "*** PyAst: unrecognized kind of expr: %s\n" unknown_type in
        None
      
      | _ ->
        None and
  
  (* TODO: Combine with other parse_*_list functions *)
  parse_expr_list json =
    match json with
      | `List item_jsons ->
        Option.all (List.map item_jsons parse_expr) >>= fun items ->
        Some items
      
      | _ ->
        None and
  
  (* TODO: Combine with other parse_*_option functions *)
  parse_expr_option json =
    match json with
      | `Null ->
        Some None
      
      | _ ->
        parse_expr json >>= fun expr ->
        Some (Some expr) and
  
  parse_location attributes_json =
    match attributes_json with
      | `Assoc [("lineno", `Int lineno); ("col_offset", `Int col_offset)] ->
        Some { lineno = lineno; col_offset = col_offset }
      
      | _ ->
        None and
  
  parse_expr_context json =
    match json with
      | `List [`String "Load"; `Assoc []]     -> Some Load
      | `List [`String "Store"; `Assoc []]    -> Some Store
      | `List [`String "Del"; `Assoc []]      -> Some Del
      | `List [`String "AugLoad"; `Assoc []]  -> Some AugLoad
      | `List [`String "AugStore"; `Assoc []] -> Some AugStore
      | `List [`String "Param"; `Assoc []]    -> Some Param
      | _                                     -> None and
  
  parse_keyword json =
    match json with
      | `List [`String "keyword"; members_json] ->
        let arg_json      = members_json |> member "arg" in
        let value_json    = members_json |> member "value" in
        
        parse_identifier    arg_json        >>= fun arg ->
        parse_expr          value_json      >>= fun value ->
        
        Some {
          arg = arg;
          value = value
        }
      
      | _ ->
        None and
  
  parse_keyword_list json =
    match json with
      | `List item_jsons ->
        Option.all (List.map item_jsons parse_keyword) >>= fun items ->
        Some items
      
      | _ ->
        None and
  
  (* === Parse Builtin Types === *)
  
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
        None


(* === Parse AST from Python Source === *)

(** Parses a .py file to a JSON AST. *)
let (parse_ast_of_file_as_json : string -> Yojson.Basic.json) py_filepath =
  (* TODO: Escape shell metacharacters and similar *)
  let shell_command = "python3 src/parse_ast.py " ^ py_filepath in
  let json_string = Subprocess.check_output shell_command in
  let json = Yojson.Basic.from_string json_string in
  json


(** Parses a .py file to an `ast`. *)
let (parse_ast_of_file : string -> ast option) py_filepath =
  let json = parse_ast_of_file_as_json py_filepath in
  parse_ast json

(* === AST Utilities === *)

(** Formats an ast as a printable string. *)
let (string_of_ast : ast -> string) ast =
  Sexp.to_string (sexp_of_ast ast)
