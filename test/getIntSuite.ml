open TypeSafeConfig
open OUnit2
open TestTypes

let test_get_int _ =
  let config = person_to_config person1 in
  assert_equal person1.p_age (get_int config "person.age")

let test_get_int_list _ =
  let config = server_to_config server1 in
  assert_equal server1.s_ports (get_int_list config "server.ports")

let suite = "GetIntSuite" >::: [
  "test_get_int" >:: test_get_int;
  "test_get_int_list" >:: test_get_int_list;
]

