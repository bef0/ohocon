type t = (string * Hocon.t) list

type key = string

let create pairs = pairs

let get pairs key =
  match List.find_opt (fun (k, _) -> String.equal k key) pairs with
  | Some (_, v) -> v
  | None -> raise Not_found

let unwrapped pairs = pairs
