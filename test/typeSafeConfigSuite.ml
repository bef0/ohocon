open TypeSafeConfig
open OUnit2
open TestTypes

let test_get_config _ =
  let config  = person_to_config person1 in
  let config' = get_config config "person" in
  assert_equal person1.p_name (get_string config' "name")

let test_with_fallback _ =
  let config = with_fallback (person_to_config person1) (server_to_config server1) in
  assert_equal ~cmp:compare_list server1.s_ports (get_int_list config "server.ports")

let test_resolve _ =
  let config  = TypeSafeConfigFactory.parse_string "{ name1 = \"bar\", name2 = ${name1} }" in
  let config' = TypeSafeConfig.resolve config in
  assert_equal "bar" (get_string config' "name2")

let test_get_is_null _ =
  let config = TypeSafeConfigFactory.parse_string "{ var1 = null, var2 = 3 }" in
  begin
    assert_equal true (get_is_null config "var1");
    assert_equal false (get_is_null config "var2");
  end

    
let suite = "TypeSafeConfigSuite" >::: [
  "test_get_config" >:: test_get_config;
  "test_with_fallback" >:: test_with_fallback;
  "test_resolve" >:: test_resolve;
  "test_get_is_null" >:: test_get_is_null;
]
