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
  | HoconBool of bool
  | HoconReference of path_expr
  | HoconString of string
  | HoconInt of int
  | HoconArray of t list
  | HoconObject of (path * t) list

val null : t

val of_bool : bool -> t

val of_path_expr : path_expr -> t

val of_string : string -> t

val of_int : int -> t

val of_list : t list -> t

val of_int_list : int list -> t

val of_string_list : string list -> t

val of_tuples : (path * t) list -> t

val dump : t -> unit
