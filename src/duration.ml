open Stdint

type duration_unit =
  | Nano
  | Milli
  | Second
  | Minute
  | Hour
  | Day

type t = Stdint.uint64 * duration_unit

let of_nano x = x, Nano

let of_milli x = x, Milli

let of_second x = x, Second
                  
let of_minute x = x, Minute

let of_hour x = x, Hour

let of_day x = x, Day

let to_nano (x, d) =
  match d with
  | Nano   -> x
  | Milli  -> Uint64.mul x (Uint64.of_int 1000000)
  | Second -> Uint64.mul x (Uint64.of_int 1000000000)
  | Minute -> Uint64.mul x (Uint64.of_string "60000000000")
  | Hour   -> Uint64.mul x (Uint64.of_string "3600000000000")
  | Day    -> Uint64.mul x (Uint64.of_string "86400000000000")

let create time duration_unit = time, duration_unit

let compare d1 d2 =
  match d1, d2 with
  | (x, Nano),   (y, Nano)   -> Stdint.Uint64.compare x y
  | (x, Milli),  (y, Milli)  -> Stdint.Uint64.compare x y
  | (x, Second), (y, Second) -> Stdint.Uint64.compare x y
  | (x, Minute), (y, Minute) -> Stdint.Uint64.compare x y
  | (x, Hour),   (y, Hour)   -> Stdint.Uint64.compare x y
  | (x, Day),    (y, Day)    -> Stdint.Uint64.compare x y
  | _                        -> -1

let equal d1 d2 = compare d1 d2 = 0

let zero = (Uint64.zero, Nano)
