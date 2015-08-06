(* core: -package core -thread *)
(* sexplib: -package sexplib.syntax -syntax camlp4o *)
(* yojson: -package yojson *)
(* <<permit duplicate labels>>: -w -30 *) 

open Core.Std
open Option.Monad_infix
open Yojson.Basic.Util

(* === AST Types === *)
type
  (* NOTE: Can't name "mod" because it is an OCaml keyword *)
  mod_ =
    | Module of mod_Module and
  mod_Module = { body : stmt list } and

  stmt =
    | Expr of stmt_Expr and
  stmt_Expr = { value : expr } and

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
  
  (* ASDL's six builtin types are:
   * identifier, int, string, bytes, object, singleton *)
  identifier = string

  with sexp

let sexp_of_mod = sexp_of_mod_


(* === AST Parse from JSON === *)
let rec
  parse_mod json =
    match json with
      | `List [`String "Module"; members_json] ->
        let body_json     = members_json |> member "body" in
        
        parse_stmt_list    body_json       >>= fun body ->
        
        Some (Module { body = body })
      
      | _ ->
        None and
  
  parse_stmt json =
    match json with
      | `List [`String "Expr"; members_json] ->
        let value_json    = members_json |> member "value" in
        
        parse_expr          value_json      >>= fun value ->
        
        Some (Expr { value = value })
      
      | _ ->
        None and
  
  (* TODO: Generify to parse lists of Parseables in general *)
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
      
      | `List [`String "Name"; members_json] ->
        let id_json       = members_json |> member "id" in
        let ctx_json      = members_json |> member "ctx" in
        
        parse_identifier    id_json         >>= fun id ->
        parse_expr_context  ctx_json        >>= fun ctx ->
        
        Some (Name { id = id; ctx = ctx })
      
      | _ ->
        None and
  
  parse_expr_list json =
    Some [Str { s = "Hello" }] and
  
  parse_expr_option json =
    Some (None) and
  
  parse_expr_context json =
    Some Load and
  
  parse_keyword_list json =
    Some [] and
  
  parse_identifier json =
    Some "print"


(* === Main === *)
let () =
  let json = Yojson.Basic.from_file "test_data/hello.py.ast" in
  
  (* Print parsed AST *)
  let () = match (parse_mod json) with
    | Some parsed ->
      printf "%s\n" (Sexp.to_string (sexp_of_mod parsed))
    
    | None ->
      printf "Parse failed\n" in
  
  (* Print JSON *)
  printf "%s\n" (Yojson.Basic.to_string json)
