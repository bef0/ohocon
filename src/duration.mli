type t
  
type duration_unit =
  | Nano
  | Milli
  | Second
  | Minute
  | Hour
  | Day

val of_nano : Stdint.Uint64.t -> t

val of_milli : Stdint.Uint64.t -> t

val of_second : Stdint.Uint64.t -> t

val of_minute : Stdint.Uint64.t -> t

val of_hour : Stdint.Uint64.t -> t

val of_day : Stdint.Uint64.t -> t

val to_nano : t -> Stdint.Uint64.t

val create : Stdint.Uint64.t -> duration_unit -> t

val compare : t -> t -> int

val equal : t -> t -> bool

val zero : t
