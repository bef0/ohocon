open TypeSafeConfig
open OUnit2
open TestTypes

let test_get_bool _ =
  let config = TypeSafeConfigFactory.parse_string "{ var1 = true, var2 = false }" in
  begin
    assert_equal (Some true) (get_bool_opt config "var1");
    assert_equal (Some false) (get_bool_opt config "var2");
    assert_equal true (get_bool config "var1");
    assert_equal false (get_bool config "var2");
  end

let suite = "GetBoolSuite" >::: [
  "test_get_bool" >:: test_get_bool;
]
