open OUnit2

let test_cases =
  "test processing" >::: [("test files" >:: fun _ -> assert_equal "abc" "abc")]

let _ = run_test_tt_main test_cases
