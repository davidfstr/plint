open OUnit

(* -------------------------------------------------------------------------- *)
(* Subprocess *)

exception ProcessExitedNonZero of int
exception ProcessStopped of int
exception ProcessKilled of int

(** Runs command `cmd` and returns its output as a string. *)
let check_output cmd =
  let ic, oc = Unix.open_process cmd in
  let output_buf = Buffer.create 16 in
  (try
     while true do
       Buffer.add_channel output_buf ic 1
     done
   with End_of_file -> ());
  let exit_status = Unix.close_process (ic, oc) in
  let () = match exit_status with
    | Unix.WEXITED 0
      -> ()
    | Unix.WEXITED return_code
      -> raise (ProcessExitedNonZero return_code)
    | Unix.WSIGNALED signal_num
      -> raise (ProcessKilled signal_num)
    | Unix.WSTOPPED signal_num
      -> raise (ProcessStopped signal_num) in
  (Buffer.contents output_buf)

(* -------------------------------------------------------------------------- *)

let test_fixture = "Plint" >:::
[
  "test_can_parse_hello" >:: ( fun () ->
    let expected_ast = BatFile.with_file_in "test_data/hello.py.ast" BatIO.read_all in
    let actual_ast = check_output "python3 parse_ast.py test_data/hello.py" in
    assert_equal expected_ast actual_ast
  )
]

let _ = run_test_tt test_fixture
