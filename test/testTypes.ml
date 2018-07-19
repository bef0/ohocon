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
  s_timeout : Duration.t;
  s_intervals : Duration.t list;
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
       ("timeout", H.of_duration s.s_timeout);
       ("intervals", H.of_list (List.map H.of_duration s.s_intervals));
     ]
   )
  )
])

let person1 = {
  p_name = "john"; p_age = 30;
  p_emails = ["a@example.com"; "b@example.com";]
}

let server1 = {
  s_ports = [80; 8080;];
  s_timeout = Duration.create (Stdint.Uint64.of_int 3) Duration.Second;
  s_intervals = [
    Duration.create (Stdint.Uint64.of_int 1) Duration.Nano;
    Duration.create (Stdint.Uint64.of_int 2) Duration.Milli;
    Duration.create (Stdint.Uint64.of_int 3) Duration.Second;
    Duration.create (Stdint.Uint64.of_int 4) Duration.Minute;
    Duration.create (Stdint.Uint64.of_int 5) Duration.Hour;
    Duration.create (Stdint.Uint64.of_int 5) Duration.Day;
  ]
}

let compare_list l1 l2 = List.for_all2 (=) l1 l2
