type ('r, 'a) t

val return : 'a -> ('r, 'a) t

val cont : (('a -> 'r) -> 'r) -> ('r, 'a) t

val map : ('r, 'a) t -> ('r -> 'r) -> ('r, 'a) t

val run_cont : ('r, 'a) t -> ('a -> 'r) -> 'r
