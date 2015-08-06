(* core: -package core -thread *)
(* sexplib: -package sexplib.syntax -syntax camlp4o *)
(* yojson: -package yojson *)

open Core.Std
open Option.Monad_infix
open Yojson.Basic.Util


type pmodule = { body : pstmt list }


 and pstmt =
  | PExpr of pstmt_PExpr
  
 and pstmt_PExpr = { expr_value : pexpr }
 
 
 and pexpr_context = Load | Store | Del | AugLoad | AugStore | Param


 and pexpr =
  | PCall of pexpr_PCall
  | PStr of pexpr_PStr
  | PName of pexpr_PName

 and pexpr_PCall =
  { func : pexpr;
    args : pexpr list;
    keywords : pkeyword list;
    starargs : pexpr option;
    kwargs : pexpr option }

 and pexpr_PStr = { s : string }
 
 and pexpr_PName = { id : pidentifier; ctx : pexpr_context }


 and pkeyword = { arg : pidentifier; keyword_value : pexpr }

 and pidentifier = string

 with sexp


let parse_expr_list json =
  Some [PStr { s = "Hello" }]


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
      
      Some (PCall { func = func;
                    args = args;
                    keywords = keywords;
                    starargs = starargs;
                    kwargs = kwargs })
    
    | `List [`String "Name"; members_json] ->
      let id_json       = members_json |> member "id" in
      let ctx_json      = members_json |> member "ctx" in
      
      parse_identifier    id_json         >>= fun id ->
      parse_expr_context  ctx_json        >>= fun ctx ->
      
      Some (PName { id = id; ctx = ctx })
    
    | _ ->
      None


let parse_pstmt json =
  match json with
    | `List [`String "Expr"; members_json] ->
      let value_json    = members_json |> member "value" in
      
      parse_expr          value_json      >>= fun value ->
      
      Some (PExpr { expr_value = value })
    
    | _ ->
      None


(* TODO: Generify to parse lists of Parseables in general *)
let parse_pstmt_list json =
  match json with
    | `List item_jsons ->
      Option.all (List.map item_jsons parse_pstmt) >>= fun items ->
      Some items
    
    | _ ->
      None


let parse_pmodule json =
  match json with
    | `List [`String "Module"; members_json] ->
      let body_json     = members_json |> member "body" in
      
      parse_pstmt_list    body_json       >>= fun body ->
      
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
