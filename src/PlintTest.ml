open OUnit

let test_fixture = "Plint" >:::
[
  (* Parse AST *)
  
  "test_can_parse_simple_ast_as_json_in_expected_format" >:: ( fun () ->
    let expected_ast_json = Yojson.Basic.from_file "src/test_data/ok_01_print.py.ast" in
    let actual_ast_json = PyAst.parse_ast_of_file_as_json "src/test_data/ok_01_print.py" in
    assert_equal ~printer:Yojson.Basic.to_string
      expected_ast_json actual_ast_json
  );
  
  "test_can_parse_simple_ast" >:: ( fun () ->
    let ast_option = PyAst.parse_ast_of_file "src/test_data/ok_01_print.py" in
    
    let open PyAst in
    let (expected_ast : PyAst.ast) = 
      [
        Expr {
          value = Call {
            func = Name {
              id = "print";
              ctx = Load;
              location = { lineno = 1; col_offset = 1 }
            };
            args = [
              Str {
                s = "Hello";
                location = { lineno = 1; col_offset = 1 }
              }
            ];
            keywords = [];
            starargs = None;
            kwargs = None;
            location = { lineno = 1; col_offset = 1 }
          };
          location = { lineno = 1; col_offset = 1 }
        }
      ] in
    
    match ast_option with
      | Some actual_ast ->
        assert_equal ~printer:PyAst.string_of_ast expected_ast actual_ast
      
      | None ->
        assert_failure (
          "Could not parse generated AST JSON. " ^
          "Maybe missing some parse rules in PyAstGrammar.ml?"
        )
  );
  
  (* Error Reporting *)
  
  "test_passes_simple_program" >:: ( fun () ->
    assert_equal ~printer:Plint.string_of_error_list
      []
      (Plint.check "src/test_data/ok_01_print.py")
  );
  
  "test_flags_misspelled_function_invocation" >:: ( fun () ->
    let open Plint in
    assert_equal ~printer:Plint.string_of_error_list
      [
        {
          line = 1;
          exn = "NameError: name 'prnt' is not defined"
        }
      ]
      (Plint.check "src/test_data/bad_01_prnt.py")
  );
  
  "test_flags_multiple_errors" >:: ( fun () ->
    let actual_errors = Plint.check "src/test_data/bad_02_prnt_prnt.py" in
    
    let open Plint in
    let expected_errors = [
      {
        line = 1;
        exn = "NameError: name 'prnt' is not defined"
      };
      {
        line = 2;
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
  
  (* Assignment & Variables *)
  
  "test_passes_assignment" >:: ( fun () ->
    assert_equal ~printer:Plint.string_of_error_list
      []
      (Plint.check "src/test_data/ok_02_assignment.py")
  );
  
  "test_passes_read_of_assigned_var" >:: ( fun () ->
    assert_equal ~printer:Plint.string_of_error_list
      []
      (Plint.check "src/test_data/ok_03_read_assigned_var.py")
  );
  
  "test_flags_read_of_unassigned_var" >:: ( fun () ->
    let open Plint in
    assert_equal ~printer:Plint.string_of_error_list
      [
        {
          line = 1;
          exn = "NameError: name 'x' is not defined"
        }
      ]
      (Plint.check "src/test_data/bad_03_read_unassigned_var.py")
  );
  
  "test_flags_read_of_unassigned_self" >:: ( fun () ->
    let open Plint in
    assert_equal ~printer:Plint.string_of_error_list
      [
        {
          line = 1;
          exn = "NameError: name 'x' is not defined"
        }
      ]
      (Plint.check "src/test_data/bad_04_read_unassigned_self.py")
  );
  
  "test_passes_del" >:: ( fun () ->
    assert_equal ~printer:Plint.string_of_error_list
      []
      (Plint.check "src/test_data/ok_04_del.py")
  );
  
  "test_flags_read_of_deleted_var" >:: ( fun () ->
    let open Plint in
    assert_equal ~printer:Plint.string_of_error_list
      [
        {
          line = 3;
          exn = "NameError: name 'x' is not defined"
        }
      ]
      (Plint.check "src/test_data/bad_05_read_deleted_var.py")
  );
  
  "test_passes_augmented_assignment" >:: ( fun () ->
    assert_equal ~printer:Plint.string_of_error_list
      []
      (Plint.check "src/test_data/ok_05_augmented_assignment.py")
  );
  
  (* Conditionals *)
  
  "test_passes_conditional" >:: ( fun () ->
    assert_equal ~printer:Plint.string_of_error_list
      []
      (Plint.check "src/test_data/ok_06_conditional.py")
  );
  
  "test_flags_read_of_potentially_unassigned_var_in_if" >:: ( fun () ->
    let open Plint in
    assert_equal ~printer:Plint.string_of_error_list
      [
        {
          line = 5;
          exn = "NameError: name 'x' is not defined"
        }
      ]
      (Plint.check "src/test_data/bad_06_read_potentially_unassigned_var_in_if.py")
  );
  
  "test_flags_errors_in_all_parts_of_conditional" >:: ( fun () ->
    let open Plint in
    assert_equal ~printer:Plint.string_of_error_list
      [
        {
          line = 1;
          exn = "NameError: name 'missing' is not defined"
        };
        {
          line = 3;
          exn = "NameError: name 'missing' is not defined"
        };
        {
          line = 5;
          exn = "NameError: name 'missing' is not defined"
        };
        {
          line = 6;
          exn = "NameError: name 'missing' is not defined"
        }
      ]
      (Plint.check "src/test_data/bad_07_errors_in_if.py")
  );
  
  (* Loops *)
  
  "test_passes_while_loop" >:: ( fun () ->
    assert_equal ~printer:Plint.string_of_error_list
      []
      (Plint.check "src/test_data/ok_07_loop.py")
  );
  
  "test_flags_read_of_potentially_unassigned_var_in_loop" >:: ( fun () ->
    let open Plint in
    assert_equal ~printer:Plint.string_of_error_list
      [
        {
          line = 4;
          exn = "NameError: name 'y' is not defined"
        }
      ]
      (Plint.check "src/test_data/bad_08_read_potentially_unassigned_var_in_loop.py")
  );
  
  "test_flags_error_in_loop_only_once" >:: ( fun () ->
    let open Plint in
    assert_equal ~printer:Plint.string_of_error_list
      [
        {
          line = 4;
          exn = "NameError: name 'missing' is not defined"
        }
      ]
      (Plint.check "src/test_data/bad_09_error_in_loop_reported_once.py")
  );
  
  "test_flags_errors_in_all_parts_of_loop" >:: ( fun () ->
    let open Plint in
    assert_equal ~printer:Plint.string_of_error_list
      [
        {
          line = 2;
          exn = "NameError: name 'missing' is not defined"
        };
        {
          line = 5;
          exn = "NameError: name 'missing' is not defined"
        };
        {
          line = 8;
          exn = "NameError: name 'missing' is not defined"
        };
        {
          line = 9;
          exn = "NameError: name 'missing' is not defined"
        }
      ]
      (Plint.check "src/test_data/bad_10_errors_in_loop.py")
  );
]


let _ =
  run_test_tt test_fixture
