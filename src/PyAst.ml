(* core: -package core -thread *)
(* sexplib: -package sexplib.syntax -syntax camlp4o *)
(* yojson: -package yojson *)
(* <<permit duplicate labels>>: -w -30 *) 

open Core.Std
open Option.Monad_infix
open Yojson.Basic.Util


(* NOTE: Can't name "module" because it is an OCaml keyword *)
type pmodule = { body : stmt list }


 and stmt =
  | Expr of stmt_Expr
  
 and stmt_Expr = { value : expr }
 
 
 and expr_context = Load | Store | Del | AugLoad | AugStore | Param


 and expr =
  | Call of expr_Call
  | Str of expr_Str
  | Name of expr_Name

 and expr_Call =
  { func : expr;
    args : expr list;
    keywords : pkeyword list;
    starargs : expr option;
    kwargs : expr option }

 and expr_Str = { s : string }
 
 and expr_Name = { id : pidentifier; ctx : expr_context }


 and pkeyword = { arg : pidentifier; value : expr }

 and pidentifier = string

 with sexp


let parse_expr_list json =
  Some [Str { s = "Hello" }]


let parse_keyword_list json =
  Some []


let parse_expr_option json =
  Some (None)


let parse_identifier json =
  Some "print"


let parse_expr_context json =
  Some Load


let rec parse_expr json =
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
      
      Some (Call { func = func;
                   args = args;
                   keywords = keywords;
                   starargs = starargs;
                   kwargs = kwargs })
    
    | `List [`String "Name"; members_json] ->
      let id_json       = members_json |> member "id" in
      let ctx_json      = members_json |> member "ctx" in
      
      parse_identifier    id_json         >>= fun id ->
      parse_expr_context  ctx_json        >>= fun ctx ->
      
      Some (Name { id = id; ctx = ctx })
    
    | _ ->
      None


let parse_stmt json =
  match json with
    | `List [`String "Expr"; members_json] ->
      let value_json    = members_json |> member "value" in
      
      parse_expr          value_json      >>= fun value ->
      
      Some (Expr { value = value })
    
    | _ ->
      None


(* TODO: Generify to parse lists of Parseables in general *)
let parse_stmt_list json =
  match json with
    | `List item_jsons ->
      Option.all (List.map item_jsons parse_stmt) >>= fun items ->
      Some items
    
    | _ ->
      None


let parse_pmodule json =
  match json with
    | `List [`String "Module"; members_json] ->
      let body_json     = members_json |> member "body" in
      
      parse_stmt_list    body_json       >>= fun body ->
      
      Some { body = body }
    
    | _ ->
      None


let () =
  let json = Yojson.Basic.from_file "test_data/hello.py.ast" in
  
  (* Print parsed AST *)
  let () = match (parse_pmodule json) with
    | Some parsed ->
      printf "%s\n" (Sexp.to_string (sexp_of_pmodule parsed))
    
    | None ->
      printf "Parse failed\n" in
  
  (* Print JSON *)
  printf "%s\n" (Yojson.Basic.to_string json)
