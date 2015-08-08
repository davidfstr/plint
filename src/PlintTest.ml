open OUnit

let test_fixture = "Plint" >:::
[
  (* Parse AST *)
  
  "test_can_parse_simple_ast_as_json_in_expected_format" >:: ( fun () ->
    let expected_ast_json = Yojson.Basic.from_file "src/test_data/ok_1_print.py.ast" in
    let actual_ast_json = PyAst.parse_ast_of_file_as_json "src/test_data/ok_1_print.py" in
    assert_equal ~printer:Yojson.Basic.to_string
      expected_ast_json actual_ast_json
  );
  
  "test_can_parse_simple_ast" >:: ( fun () ->
    let ast_option = PyAst.parse_ast_of_file "src/test_data/ok_1_print.py" in
    
    let open PyAst in
    let (expected_ast : ast) = 
      [
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
      ] in
    
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
    let actual_errors = Plint.check "src/test_data/ok_1_print.py" in
    let expected_errors = [] in
    
    assert_equal ~printer:Plint.string_of_error_list
      expected_errors actual_errors
  );
  
  "test_flags_misspelled_function_invocation" >:: ( fun () ->
    let actual_errors = Plint.check "src/test_data/bad_1_prnt.py" in
    
    let open Plint in
    let expected_errors = [
      {
        line = 1;
        exn = "NameError: name 'prnt' is not defined"
      }
    ] in
    
    assert_equal ~printer:Plint.string_of_error_list
      expected_errors actual_errors
  );
  
  (* Error Formatting *)
  
  "test_can_format_simple_error_correctly" >:: ( fun () ->
    let open Plint in
    let error = {
      line = 1;
      exn = "NameError: name 'prnt' is not defined"
    } in
    let actual_description = Plint.descripton_of_error error in
    
    let expected_description = 
      "Traceback (most recent call last):\n" ^
      "  Line 1, in <module>\n" ^
      "    prnt('Hello')\n" ^
      "NameError: name 'prnt' is not defined" in
    
    assert_equal ~printer:BatPervasives.identity
      expected_description actual_description
  );
]


let _ =
  run_test_tt test_fixture
