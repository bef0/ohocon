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
  let compare_fragment p1 p2 = (String.compare p1 p2) = 0 in
  match fragments with
  | [] -> None
  | p1 :: rest -> begin
      match t with
      | Leaf(p2, v) when compare_fragment p1 p2 -> f v
      | Branch(p2, nodes) when compare_fragment p1 p2 ->
        begin
          (* TODO *)
          match List.map (fun n -> get_value_opt n rest f)  nodes with
          | [] -> None
          | hd :: _ -> hd
        end
      | _ -> None
    end

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
