open TypeSafeConfig
open OUnit2
open TestTypes

let test_get_float_with_frac _ =
  let config = TypeSafeConfigFactory.parse_string "price=3.1" in
  assert_equal 3.1 (get_float config "price")

let test_get_float_with_exp _ =
  let config = TypeSafeConfigFactory.parse_string "{ price1=10.32e+0, price2=8.329E-1 }" in
  begin
    assert_equal 10.32 (get_float config "price1");
    assert_equal 0.8329 (get_float config "price2");
  end

let suite = "GetFloatSuite" >::: [
  "test_get_float_with_frac" >:: test_get_float_with_frac;
  "test_get_float_with_exp" >:: test_get_float_with_exp;
]

