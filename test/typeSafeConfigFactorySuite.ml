open TypeSafeConfig
open OUnit2
open TestTypes

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

let suite = "TypeSafeConfigFactorySuite" >::: [
  "test_from_string" >:: test_from_string;
]
