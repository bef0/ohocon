let map x f = match x with
  | Some v -> Some (f v)
  | None -> None
    
let is_defined = function
  | Some _ -> true
  | None -> false
    
let fold x none f = match x with
  | Some x' -> f x'
  | None -> Lazy.force none
              
let flatten values =
  let rec loop acc xs =
    match xs with
    | [] -> acc
    | (Some x) :: xs' -> loop (x :: acc) xs'
    | None :: xs'   -> loop acc xs'
  in List.rev (loop [] values)
