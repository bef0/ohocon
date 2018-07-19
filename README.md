OHocon
------

OHocon is a port of [lightbend's config library](https://github.com/lightbend/config).

# Tutorial

## create a config object

First, you must create a `TypeSafeConfig` object by parsing an external resource(e.g. a file).

```ocaml
open OHocon

(* load from a string *)
let config = TypeSafeConfigFactory.parse_string "{name:\"john\"}"
```

## string

```ocaml
open OHocon

let config = TypeSafeConfigFactory.parse_string "{name:\"john\",countries:[\"jp\", \"us\"]}"

(* get string. Throw an exception if none. *)
let _ = TypeSafeConfig.get_string "name"

(* get string as string option. *)
let _ = TypeSafeConfig.get_string_opt "name"

(* get a list of string. *)
let _ = TypeSafeConfig.get_string_list "countries"
```

## int

```ocaml
open OHocon

let config = TypeSafeConfigFactory.parse_string "{ id: 1, follower_ids: [2,3,4] }"

(* get int. Throw an exception if none. *)
let _ = TypeSafeConfig.get_int "id"

(* get int as int option. *)
let _ = TypeSafeConfig.get_int_opt "id"

(* get a list of int. *)
let _ = TypeSafeConfig.get_int_list "follower_ids"
```
