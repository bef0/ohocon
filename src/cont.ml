type ('r, 'a) t = ('a -> 'r) -> 'r

let return x = (fun k -> k x)

let cont f = f

let map t f = (fun k -> f (t k))

let run_cont t init = t init
