exception FileNotFoundException of string

type path = string

val empty : unit -> TypeSafeConfig.t

val parse_string : string -> TypeSafeConfig.t

val parse_file : path -> TypeSafeConfig.t
