type path = string

type path_expr = string list * bool

type t =
  | HoconNull
  | HoconBool of bool
  | HoconReference of path_expr
  | HoconString of string
  | HoconInt of int
  | HoconFloat of float
  | HoconArray of t list
  | HoconDuration of Duration.t
  | HoconObject of (path * t) list

let null = HoconNull

let of_bool b = HoconBool b

let of_float n = HoconFloat n

let of_path_expr e = HoconReference e

let of_string s = HoconString s

let of_int n = HoconInt n

let of_list es = HoconArray es

let of_int_list ns = HoconArray (List.map (fun n -> HoconInt n) ns)

let of_string_list ss = HoconArray (List.map (fun s -> HoconString s ) ss)

let of_tuples ts = HoconObject ts

let of_duration duration = HoconDuration duration

let rec dump = function
  | HoconObject pairs -> begin
      print_endline "HoconObject";
      dump_pairs pairs;
    end
  | HoconString s -> begin
      print_endline "HoconString";
      print_endline s;
    end
  | HoconReference _ -> begin
      print_endline "HoconReference";
    end
  | _ -> ()

and dump_pairs = function
  | [] -> print_string "[]"
  | (frag, t) :: pairs -> begin
      print_string frag;
      dump t;
      dump_pairs pairs;
    end
