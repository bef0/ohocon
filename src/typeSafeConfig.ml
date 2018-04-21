type value = 
  | HoconString of string
  | HoconInt of int
  | HoconStringList of string list
  | HoconIntList of int list

and tree =
  | Root
  | Leaf of path * value
  | Branch of path * tree list

and path = string
 
type t = tree

exception ConfigMissing of string

let create () = Root

let parse_path_fragments path = String.split_on_char '.' path

let rec get_value_opt t fragments f =
  let rec compare_fragment p1 p2 = (String.compare p1 p2) = 0 in
  let rec find_first nodes frags =
    match nodes with
    | [] -> None
    | t1 :: rest -> begin
        match get_value t1 frags with
        | Some x -> Some x
        | None -> find_first rest frags
      end
  and get_value tree frags =
    match frags with
    | [] -> None
    | p1 :: rest -> begin
        match tree with
        | Leaf(p2, v) when compare_fragment p1 p2 -> f v
        | Branch(p2, nodes) when compare_fragment p1 p2 -> find_first nodes rest
        | _ -> None
      end
  in get_value t fragments
       
let get_string_opt t path =
  get_value_opt t (parse_path_fragments path) (function
    | HoconString x -> Some x
    | _ -> None
)

let get_string t path =
  match get_string_opt t path with
  | Some s -> s
  | None -> raise (ConfigMissing (Printf.sprintf "%s missing" path))

let get_int_opt t path =
  get_value_opt t (parse_path_fragments path) (function
    | HoconInt x -> Some x
    | _ -> None
  )

let get_string_list_opt t path =
  get_value_opt t (parse_path_fragments path) (function
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
  get_value_opt t (parse_path_fragments path) (function
    | HoconIntList x -> Some x
    | _ -> None
  )

let get_int_list t path =
  match get_int_list_opt t path with
  | Some x -> x
  | None -> raise (ConfigMissing (Printf.sprintf "%s missing" path))
