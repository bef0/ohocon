open TypeSafeConfig
open OUnit2
module H = Hocon
module C = TypeSafeConfig
module F = TypeSafeConfigFactory

type person = {
  p_name   : string;
  p_age    : int;
  p_emails : string list;
}

type server = {
  s_ports : int list;
}

let person_to_config p = C.of_value(H.of_tuples [
  ("person", H.of_tuples(
     [
       ("name", H.of_string p.p_name);
       ("age", H.of_int p.p_age);
       ("emails", (H.of_string_list p.p_emails));
     ]
   )
  )
])

let server_to_config s = C.of_value(H.of_tuples [
  ("server", H.of_tuples(
     [
       ("ports", H.of_int_list s.s_ports);
     ]
   )
  )
])

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

let test_get_is_null _ =
  let config = TypeSafeConfigFactory.parse_string "{ var1 = null, var2 = 3 }" in
  begin
    assert_equal true (get_is_null config "var1");
    assert_equal false (get_is_null config "var2");
  end

let test_get_bool _ =
  let config = TypeSafeConfigFactory.parse_string "{ var1 = true, var2 = false }" in
  begin
    assert_equal (Some true) (get_bool_opt config "var1");
    assert_equal (Some false) (get_bool_opt config "var2");
    assert_equal true (get_bool config "var1");
    assert_equal false (get_bool config "var2");
  end

let test_resolve _ =
  let config  = TypeSafeConfigFactory.parse_string "{ name1 = \"bar\", name2 = ${name1} }" in
  let config' = TypeSafeConfig.resolve config in
  assert_equal "bar" (get_string config' "name2")
    
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

let test_get_object _ =
  let module O = ConfigObject in
  let config  = TypeSafeConfigFactory.parse_string "{ person.age = 42 }" in
  let v1 = match (C.get_object config "person") |> (fun t -> O.get t "age") with
    | HoconInt n -> 42
    | _ -> 0
  in
  let v2 = match (C.get_object_opt config "person") with
    | Some obj -> begin
        match O.get obj "age" with
        | HoconInt n -> Some n
        | _ -> None
      end
    | None -> None
  in
  begin
    assert_equal 42 v1;
    assert_equal (Some 42) v2;
  end

let suite = "suite" >::: [
  "test_get_config" >:: test_get_config;
  "test_resolve" >:: test_resolve;
  "test_with_fallback" >:: test_with_fallback;
  "test_get_string" >:: test_get_string;
  "test_get_int" >:: test_get_int;
  "test_get_string_list" >:: test_get_string_list;
  "test_get_int_list" >:: test_get_int_list;
  "test_get_is_null" >:: test_get_is_null;
  "test_get_bool" >:: test_get_bool;
  "test_from_string" >:: test_from_string;
  "test_get_object" >:: test_get_object;
]

let () = OUnit2.run_test_tt_main suite
