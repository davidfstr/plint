(* batteries: -package batteries *)
(* ounit: -package ounit *)

open OUnit
open Subprocess


let test_fixture = "Plint" >:::
[
  (* Parse AST *)
  
  "test_can_parse_simple_ast_as_json_in_expected_format" >:: ( fun () ->
    let expected_ast_json = Yojson.Basic.from_file "src/test_data/ok_1_print.py.ast" in
    let actual_ast_json = PyAst.parse_ast_of_file_as_json "src/test_data/ok_1_print.py" in
    assert_equal expected_ast_json actual_ast_json
  );
  
  "test_can_parse_simple_ast" >:: ( fun () ->
    let ast_option = PyAst.parse_ast_of_file "src/test_data/ok_1_print.py" in
    
    let open PyAst in
    let (expected_ast : ast) = 
      Module {
        body = [
          Expr {
            value = Call {
              func = Name {
                id = "print";
                ctx = Load
              };
              args = [
                Str { s = "Hello" }
              ];
              keywords = [];
              starargs = None;
              kwargs = None
            }
          }
        ]
      } in
    
    match ast_option with
      | Some actual_ast ->
        assert_equal ~printer:PyAst.string_of_ast expected_ast actual_ast
      
      | None ->
        assert_failure (
          "Could not parse generated AST JSON. " ^
          "Maybe missing some parse rules in PyAst.ml?"
        )
  );
  
  (* Error Reporting *)
  
  "test_passes_simple_program" >:: ( fun () ->
    let errors = Plint.check "src/test_data/ok_1_print.py" in
    
    assert_equal
      ~msg:"Errors incorrectly detected in simple program."
      []
      errors
  )
]


let _ =
  run_test_tt test_fixture
