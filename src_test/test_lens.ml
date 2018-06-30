open OUnit2

let suite = "Test lens" >::: [
    Test_deriving_lens.suite;
  ]

let _ =
  run_test_tt_main suite
