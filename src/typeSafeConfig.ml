open Hocon

type path = string
  
type t = Hocon.t list
  
exception ConfigMissing of string

let rec dump = function
  | [] -> ()
  | v :: vs -> begin Hocon.dump v; dump vs; end

let of_value v = [v]

let parse_path_fragments path = String.split_on_char '.' path

let as_path frags = String.concat "." frags

let compare_fragment p1 p2 = (String.compare p1 p2) = 0

let get_config_opt1 values fragments =
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
  in loop values fragments

let get_config_opt values path = get_config_opt1 values (parse_path_fragments path)

let get_config t path =
  match get_config_opt t path with
  | Some x -> x
  | None -> failwith "none"

let resolve1 origin =
  let rec loop acc = function
    | [] -> acc
    | (HoconReference(frags, is_opt)) :: ts -> begin
        match get_config_opt1 origin frags with
        | Some resolved -> loop (resolved @ acc) ts
        | None -> if is_opt then acc else
            raise (ConfigMissing (Printf.sprintf "%s missing" (as_path frags)))
      end
    | HoconObject(es) :: ts -> begin
        loop ((HoconObject (resolve_pairs es)) :: acc) ts
      end
    | t :: ts -> loop (t :: acc) ts
  and resolve_pairs = function
    | [] -> []
    | (frag, x) :: pairs -> begin
        match loop [] [x] with
        | [] -> resolve_pairs pairs
        | t :: [] -> (frag, t) :: (resolve_pairs pairs)
        | t :: _ -> failwith "bug"
      end
  in loop [] origin

let resolve t = List.rev (resolve1 t)

let with_fallback t1 t2 = List.append t1 t2

let get_value_opt t path f =
  match get_config_opt t path with
  | Some([]) -> None
  | Some(v :: _) -> f v
  | None -> None

let is_empty t = List.length t = 0

let get_object_opt t path = 
  get_value_opt t path (function
    | HoconObject pairs -> Some (ConfigObject.create pairs)
    | _ -> None
  )

let get_object t path =
  match get_object_opt t path with
  | Some obj -> obj
  | None -> raise (ConfigMissing (Printf.sprintf "%s missing" path))

let get_is_null t path =
  let res = get_value_opt t path (function
    | HoconNull -> Some true
    | _ -> None
  )
  in Option.fold res (Lazy.from_val false) Function.identity

let get_bool_opt t path =
  get_value_opt t path (function
    | HoconBool b -> Some b
    | _ -> None
  )

let get_bool t path =
  match get_bool_opt t path with
  | Some b -> b
  | None -> raise (ConfigMissing (Printf.sprintf "%s missing" path))

let get_float_opt t path =
  get_value_opt t path (function
    | HoconFloat n -> Some n
    | _ -> None
  )

let get_float t path =
  match get_float_opt t path with
  | Some n -> n
  | None -> raise (ConfigMissing (Printf.sprintf "%s missing" path))
    
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
  let f acc = function
    | HoconString e -> Option.map acc (fun es -> e :: es)
    | _ -> None
  in
  get_value_opt t path (function
    | HoconArray es -> Option.map (List.fold_left f (Some []) es) List.rev
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
  let f acc = function
    | HoconInt n -> Option.map acc (fun es -> n :: es)
    | _ -> None
  in
  get_value_opt t path (function
    | HoconArray es -> Option.map (List.fold_left f (Some []) es) List.rev
    | _ -> None
  )

let get_int_list t path =
  match get_int_list_opt t path with
  | Some x -> x
  | None -> raise (ConfigMissing (Printf.sprintf "%s missing" path))
