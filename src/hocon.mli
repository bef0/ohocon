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

val null : t

val of_bool : bool -> t

val of_float : float -> t

val of_path_expr : path_expr -> t

val of_string : string -> t

val of_int : int -> t

val of_list : t list -> t

val of_int_list : int list -> t

val of_string_list : string list -> t

val of_tuples : (path * t) list -> t

val of_duration : Duration.t -> t

val dump : t -> unit
