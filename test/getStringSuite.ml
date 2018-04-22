open TypeSafeConfig
open OUnit2
open TestTypes

let test_get_string _ =
  let config = person_to_config person1 in
  assert_equal person1.p_name (get_string config "person.name")
    
let test_get_string_list _ =
  let config = person_to_config person1 in
  assert_equal ~cmp:compare_list
    person1.p_emails
    (get_string_list config "person.emails")

let suite = "GetStringSuite" >::: [
  "test_get_string" >:: test_get_string;
  "test_get_string_list" >:: test_get_string_list;
]
