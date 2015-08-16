open Core.Std

(* Reexport all node types from the grammar *)
include PyAstGrammar

(* === Parse Source File === *)

type ast = stmt list
  with sexp

let (parse_ast : Yojson.Basic.json -> ast option) json =
  match (parse_pymod json) with
    | Some (Module { body = stmts }) ->
      Some stmts
    
    | None ->
      None

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

(** Formats an ast as a printable string. *)
let (string_of_ast : ast -> string) ast =
  Sexp.to_string (sexp_of_ast ast)
