open OUnit

let test_case =
  "test list" >::: [("test" >:: fun () -> assert_equal "abc" "abc")]

let _ = run_test_tt test_case
