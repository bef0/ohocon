open TypeSafeConfig

let () =
  let config = Leaf("name", (HoconString "hoge")) in
  begin
    print_string (get_string config "name");
    print_newline ();
  end
