open Hocon

type path = string
  
type t = Hocon.t list
  
exception ConfigMissing of string

module Option =
struct
  let map x f = match x with
    | Some v -> Some (f v)
    | None -> None
      
  let flatten values =
    let rec loop acc xs =
      match xs with
      | [] -> acc
      | (Some x) :: xs' -> loop (x :: acc) xs'
      | None :: xs'   -> loop acc xs'
    in List.rev (loop [] values)
end

let of_value v = [v]

let parse_path_fragments path = String.split_on_char '.' path

let compare_fragment p1 p2 = (String.compare p1 p2) = 0

let get_config_opt values path =
  let rec find_first p = function
    | [] -> None
    | (p', x) :: _ when compare_fragment p p' -> Some x
    | _ :: es -> find_first p es
  and non_empty fs = List.length fs <> 0
  and loop vs frags = match vs with
    | [] -> None
    | HoconObject(es) :: vs' -> begin
        match frags with
        | [] -> None
        | p1 :: ps -> begin
            match find_first p1 es with
            | Some x when non_empty ps -> loop [x] ps
            | Some x -> Some [x]
            | None -> loop vs' frags
          end
      end
    | v :: vs' -> if (non_empty frags) then loop vs' frags else Some [v]
  in loop values (parse_path_fragments path)

let get_config t path =
  match get_config_opt t path with
  | Some x -> x
  | None -> failwith "none"

let with_fallback t1 t2 = List.append t1 t2
              
let get_value_opt t path f =
  match get_config_opt t path with
  | Some([]) -> None
  | Some(v :: _) -> f v
  | None -> None
    
let get_string_opt t path =
  get_value_opt t path (function
    | HoconString x -> Some x
    | _ -> None
  )

let get_string t path =
  match get_string_opt t path with
  | Some s -> s
  | None -> raise (ConfigMissing (Printf.sprintf "%s missing" path))

let get_int_opt t path =
  get_value_opt t path (function
    | HoconInt x -> Some x
    | _ -> None
  )

let get_string_list_opt t path =
  get_value_opt t path (function
    | HoconStringList x -> Some x
    | _ -> None
  )

let get_string_list t path =
  match get_string_list_opt t path with
  | Some x -> x
  | None -> raise (ConfigMissing (Printf.sprintf "%s missing" path))

let get_int t path =
  match get_int_opt t path with
  | Some x -> x
  | None -> raise (ConfigMissing (Printf.sprintf "%s missing" path))

let get_int_list_opt t path =
  get_value_opt t path (function
    | HoconIntList x -> Some x
    | _ -> None
  )

let get_int_list t path =
  match get_int_list_opt t path with
  | Some x -> x
  | None -> raise (ConfigMissing (Printf.sprintf "%s missing" path))
