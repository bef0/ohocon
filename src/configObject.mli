type t

type key = string

val create : (key * Hocon.t) list -> t

val get : t -> key -> Hocon.t

val unwrapped : t -> (key * Hocon.t) list
