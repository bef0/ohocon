type 'a t = Success of 'a | Failure of exn

let lazily x = try Success (Lazy.force x) with e -> Failure e
              
let success x = Success x
                  
let failure e = Failure e
                  
let map t f =
  match t with
  | Success x -> begin try Success (f x) with e -> Failure e end
  | Failure e  -> Failure e
                    
let bind t f =
  match t with
  | Success x -> begin try f x with e -> Failure e end
  | Failure e -> Failure e

let recover_with t f =
  match t with
  | Success x -> Success x
  | Failure e -> begin try f e with ex -> Failure ex end

let to_option t =
  match t with
  | Success x -> Some x
  | Failure _ -> None
