(* batteries: -package batteries *)
(* ounit: -package ounit *)

open OUnit
open Subprocess


let test_fixture = "Plint" >:::
[
  "test_can_parse_hello_ast_as_json_in_expected_format" >:: ( fun () ->
    let expected_ast_json = Yojson.Basic.from_file "src/test_data/hello.py.ast" in
    let actual_ast_json = PyAst.parse_ast_of_file_as_json "src/test_data/hello.py" in
    assert_equal expected_ast_json actual_ast_json
  );
  
  "test_can_parse_hello_ast" >:: ( fun () ->
    let ast = PyAst.parse_ast_of_file "src/test_data/hello.py" in
    
    match ast with
      | Some _ ->
        ()
      
      | None   ->
        assert_failure (
          "Could not parse generated AST JSON. " ^
          "Maybe missing some parse rules in PyAst.ml?"
        )
  )
]


let _ =
  run_test_tt test_fixture
