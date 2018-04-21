type path = string
  
type t =
  | HoconNull
  | HoconString of string
  | HoconInt of int
  | HoconStringList of string list
  | HoconIntList of int list
  | HoconObject of (path * t) list

val of_string : string -> t

val of_int : int -> t

val of_int_list : int list -> t

val of_string_list : string list -> t

val of_tuples : (path * t) list -> t
