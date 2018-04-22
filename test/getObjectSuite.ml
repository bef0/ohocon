open TypeSafeConfig
open OUnit2
open TestTypes

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

let suite = "GetObjectSuite" >::: [
  "test_get_object" >:: test_get_object;
]
