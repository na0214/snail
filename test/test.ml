open OUnit2
open Snailml

let test_case = "test list" >::: [("test" >:: fun _ -> assert_equal "abc" "abc")]

let _ = run_test_tt_main test_case
