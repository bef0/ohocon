type 'a t = Success of 'a | Failure of exn

val lazily : 'a Lazy.t -> 'a t

val success : 'a -> 'a t

val failure : exn -> 'a t
                             
val map : 'a t -> ('a -> 'b) -> 'b t

val bind : 'a t -> ('a -> 'b t) -> 'b t

val recover_with : 'a t -> (exn -> 'a t) -> 'a t

val to_option : 'a t -> 'a option
