type t =
  | Empty
  | HoconString of string
  | HoconInt of int
  | HoconStringList of string list
  | HoconIntList of string list

let create () = Empty
