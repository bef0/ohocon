open TypeSafeConfig
open OUnit2
open TestTypes

let test_get_duration _ =
  let config = server_to_config server1 in
  assert_equal server1.s_timeout (get_duration config "server.timeout")

let test_get_duration_list _ =
  let config = server_to_config server1 in
  assert_equal server1.s_intervals (get_duration_list config "server.intervals")

let suite = "GetDurationSuite" >::: [
  "test_get_duration" >:: test_get_duration;
  "test_get_duration_list" >:: test_get_duration;
]
