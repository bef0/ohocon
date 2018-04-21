type path = string
  
type t =
  | HoconNull
  | HoconString of string
  | HoconInt of int
  | HoconStringList of string list
  | HoconIntList of int list
  | HoconObject of (path * t) list

let of_string s = HoconString s

let of_int n = HoconInt n

let of_int_list ns = HoconIntList ns

let of_string_list ss = HoconStringList ss

let of_tuples ts = HoconObject ts
