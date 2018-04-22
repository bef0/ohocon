type path = string

type path_expr = string list * bool

type duration_unit =
  | Nano
  | Milli
  | Second
  | Minute
  | Hour
  | Day

type duration = int64 * duration_unit
  
type t =
  | HoconNull
  | HoconReference of path_expr
  | HoconString of string
  | HoconInt of int
  | HoconStringList of string list
  | HoconIntList of int list
  | HoconObject of (path * t) list

let null = HoconNull

let of_path_expr e = HoconReference e

let of_string s = HoconString s

let of_int n = HoconInt n

let of_int_list ns = HoconIntList ns

let of_string_list ss = HoconStringList ss

let of_tuples ts = HoconObject ts

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
