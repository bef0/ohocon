open TypeSafeConfig
open OUnit2
open TestTypes
open Stdint

let test_from_string _ =
  let config1 = TypeSafeConfigFactory.parse_string "index=1" in
  let config2 = TypeSafeConfigFactory.parse_string "person.name=\"foo\"" in
  let config3 = TypeSafeConfigFactory.parse_string "person = { age: 18 }" in
  let config4 = TypeSafeConfigFactory.parse_string "password: null" in
  begin
    assert_equal 1 (get_int config1 "index");
    assert_equal "foo" (get_string config2 "person.name");
    assert_equal 18 (get_int config3 "person.age");
    assert_equal None (get_string_opt config4 "password");
  end

let test_nano_from_string _ =
  let c1 = TypeSafeConfigFactory.parse_string "interval=1ns" in
  let c2 = TypeSafeConfigFactory.parse_string "interval=1nanos" in
  List.iter (fun c ->
    assert_equal (Duration.of_nano Uint64.one) (get_duration c "interval")
  ) [c1; c2]

let test_milli_from_string _ =
  let c1 = TypeSafeConfigFactory.parse_string "interval=1ms" in
  let c2 = TypeSafeConfigFactory.parse_string "interval=1millis" in
  List.iter (fun c ->
    assert_equal (Duration.of_milli Uint64.one) (get_duration c "interval")
  ) [c1; c2]

let test_second_from_string _ =
  let c1 = TypeSafeConfigFactory.parse_string "interval=1s" in
  let c2 = TypeSafeConfigFactory.parse_string "interval=1seconds" in
  List.iter (fun c ->
    assert_equal (Duration.of_second Uint64.one) (get_duration c "interval")
  ) [c1; c2]

let test_minute_from_string _ =
  let c1 = TypeSafeConfigFactory.parse_string "interval=1m" in
  let c2 = TypeSafeConfigFactory.parse_string "interval=1minutes" in
  List.iter (fun c ->
    assert_equal (Duration.of_minute Uint64.one) (get_duration c "interval")
  ) [c1; c2]

let test_hour_from_string _ =
  let c1 = TypeSafeConfigFactory.parse_string "interval=1h" in
  let c2 = TypeSafeConfigFactory.parse_string "interval=1hours" in
  List.iter (fun c ->
    assert_equal (Duration.of_hour Uint64.one) (get_duration c "interval")
  ) [c1; c2]

let test_day_from_string _ =
  let c1 = TypeSafeConfigFactory.parse_string "interval=1d" in
  let c2 = TypeSafeConfigFactory.parse_string "interval=1days" in
  List.iter (fun c ->
    assert_equal (Duration.of_day Uint64.one) (get_duration c "interval")
  ) [c1; c2]
  
let suite = "TypeSafeConfigFactorySuite" >::: [
  "test_from_string" >:: test_from_string;
  "test_nano_from_string" >:: test_nano_from_string;
  "test_milli_from_string" >:: test_milli_from_string;
  "test_second_from_string" >:: test_second_from_string;
  "test_minute_from_string" >:: test_minute_from_string;
  "test_hour_from_string" >:: test_hour_from_string;
  "test_day_from_string" >:: test_day_from_string;
]
