(* core: -package core -thread *)
(* sexplib: -package sexplib.syntax -syntax camlp4o *)
(* yojson: -package yojson *)
(* <<permit duplicate labels>>: -w -30 *) 

open Core.Std
open Option.Monad_infix
open Yojson.Basic.Util

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
  stmt =
    | Expr of stmt_Expr and
  stmt_Expr = { value : expr } and

  (* TODO: Recognize remaining types of expr. 26 total. *)
  expr =
    | Call of expr_Call
    | Str of expr_Str
    | Name of expr_Name and
  expr_Call = {
    func : expr;
    args : expr list;
    keywords : keyword list;
    starargs : expr option;
    kwargs : expr option
  } and
  expr_Str = { s : string } and 
  expr_Name = { id : identifier; ctx : expr_context } and
  
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
      | `List [`String "Expr"; members_json] ->
        let value_json    = members_json |> member "value" in
        
        parse_expr          value_json      >>= fun value ->
        
        Some (Expr { value = value })
      
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
      | `List [`String "Call"; members_json] ->
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
        
        Some (Call {
          func = func;
          args = args;
          keywords = keywords;
          starargs = starargs;
          kwargs = kwargs
        })
      
      | `List [`String "Str"; members_json] ->
        let s_json        = members_json |> member "s" in
        
        parse_string        s_json          >>= fun s ->
        
        Some (Str { s = s })
      
      | `List [`String "Name"; members_json] ->
        let id_json       = members_json |> member "id" in
        let ctx_json      = members_json |> member "ctx" in
        
        parse_identifier    id_json         >>= fun id ->
        parse_expr_context  ctx_json        >>= fun ctx ->
        
        Some (Name { id = id; ctx = ctx })
      
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
