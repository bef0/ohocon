type t

type path = string

exception ConfigMissing of string

val of_value : Hocon.t -> t

val resolve : t -> t

val dump : t -> unit

val get_is_null : t -> path -> bool

val get_bool : t -> path -> bool

val get_bool_opt : t -> path -> bool option

val get_config_opt : t -> path -> t option

val get_config : t -> path -> t

val with_fallback : t -> t -> t

val get_string_opt : t -> path -> string option

val get_string : t -> path -> string

val get_int_opt : t -> path -> int option

val get_string_list_opt : t -> path -> string list option

val get_string_list : t -> path -> string list

val get_int : t -> path -> int

val get_int_list_opt : t -> string -> int list option

val get_int_list : t -> string -> int list
