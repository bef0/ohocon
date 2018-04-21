open TypeSafeConfig
open OUnit2

type person = {
  p_name   : string;
  p_age    : int;
  p_emails : string list;
}

type server = {
  s_ports : int list;
}

let person_to_config p = Branch(
  "person",
  [
    Leaf("name", (HoconString p.p_name));
    Leaf("age", (HoconInt p.p_age));
    Leaf("emails", (HoconStringList p.p_emails));
  ]
)

let server_to_config s = Branch(
  "server",
  [
    Leaf("ports", (HoconIntList s.s_ports));
  ]
)

let person1 = {
  p_name = "john"; p_age = 30;
  p_emails = ["a@example.com"; "b@example.com";]
}

let server1 = { s_ports = [80; 8080;]; }

let compare_list l1 l2 = List.for_all2 (=) l1 l2

let test_get_config _ =
  let config  = person_to_config person1 in
  let config' = get_config config "person" in
  assert_equal person1.p_name (get_string config' "name")

let test_with_fallback _ =
  let config = with_fallback (person_to_config person1) (server_to_config server1) in
  assert_equal ~cmp:compare_list server1.s_ports (get_int_list config "server.ports")
    
let test_get_string _ =
  let config = person_to_config person1 in
  assert_equal person1.p_name (get_string config "person.name")
    
let test_get_int _ =
  let config = person_to_config person1 in
  assert_equal person1.p_age (get_int config "person.age")

let test_get_string_list _ =
  let config = person_to_config person1 in
  assert_equal ~cmp:compare_list
    person1.p_emails
    (get_string_list config "person.emails")
    
let test_get_int_list _ =
  let config = server_to_config server1 in
  assert_equal server1.s_ports (get_int_list config "server.ports")

let suite = "suite" >::: [
  "test_get_config" >:: test_get_config;
  "test_with_fallback" >:: test_with_fallback;
  "test_get_string" >:: test_get_string;
  "test_get_int" >:: test_get_int;
  "test_get_string_list" >:: test_get_string_list;
  "test_get_int_list" >:: test_get_int_list;
]

let () = OUnit2.run_test_tt_main suite
