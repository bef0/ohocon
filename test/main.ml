open TypeSafeConfig
open OUnit2

let suite = "AllSuite" >::: [
  GetIntSuite.suite;
  GetBoolSuite.suite;
  GetStringSuite.suite;
  GetObjectSuite.suite;
  TypeSafeConfigSuite.suite;
  TypeSafeConfigFactorySuite.suite;
]

let () = OUnit2.run_test_tt_main suite
