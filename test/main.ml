open TypeSafeConfig
open OUnit2

let suite = "AllSuite" >::: [
  DurationSuite.suite;
  GetBoolSuite.suite;
  GetFloatSuite.suite;
  GetIntSuite.suite;
  GetStringSuite.suite;
  GetObjectSuite.suite;
  GetDurationSuite.suite;
  TypeSafeConfigSuite.suite;
  TypeSafeConfigFactorySuite.suite;
]

let () = OUnit2.run_test_tt_main suite
