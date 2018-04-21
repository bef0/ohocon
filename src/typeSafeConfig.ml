type value = 
  | HoconString of string
  | HoconInt of int
  | HoconStringList of string list
  | HoconIntList of int list

and tree =
  | Root of tree list
  | Leaf of path * value
  | Branch of path * tree list

and path = string
  
type t = tree
  
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

let parse_path_fragments path = String.split_on_char '.' path

let compare_fragment p1 p2 = (String.compare p1 p2) = 0

let get_config_opt root path =
  let rec loop trees frags = match trees with
    | [] -> None
    | Root(nodes) :: ts -> begin
        match loop nodes frags with
        | None -> loop ts frags
        | Some t -> Some t
      end
    | t :: ts -> begin
        match frags with
        | [] -> None
        | p1 :: rest -> begin
            match t with
            | Leaf(p2, _) when compare_fragment p1 p2 -> Some t
            | Branch(p2, nodes) when compare_fragment p1 p2 ->
              if (List.length rest = 0) then Some (Root nodes) else Some t
            | _ -> loop ts frags
          end
      end
  in loop [root;] (parse_path_fragments path)

let get_config t path =
  match get_config_opt t path with
  | Some x -> x
  | None -> failwith "none"

let with_fallback t1 t2 = Root [t1; t2]
              
let get_value_opt t path f =
  let  iter = function
    | Leaf(_, v) -> f v
    | _ -> None
  and hd_safe = function
    | [] -> None
    | v :: _ -> Some v
  in
  match get_config_opt t path with
  | Some(Leaf(_, v)) -> f v
  | Some(Branch(_, ts)) -> hd_safe (Option.flatten (List.map iter ts))
  | _ -> None

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
