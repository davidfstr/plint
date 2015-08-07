(* batteries: -package batteries *)
(* ounit: -package ounit *)

open OUnit
open Subprocess


let test_fixture = "Plint" >:::
[
  "test_can_parse_hello" >:: ( fun () ->
    let expected_ast = BatFile.with_file_in "src/test_data/hello.py.ast" BatIO.read_all in
    let actual_ast = check_output "python3 src/parse_ast.py src/test_data/hello.py" in
    assert_equal expected_ast actual_ast
  )
]

let _ = run_test_tt test_fixture
