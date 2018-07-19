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

# TODO

Items                                              | Status
---------------------------------------------------| :-----:
Comments                                           | :x:
Omit root braces                                   | :x:
Key-value separator                                | :x:
Commas                                             | :x:
Whitespace                                         | :x:
Duplicate keys and object merging                  | :x:
Unquoted strings                                   | :x:
Multi-line strings                                 | :x:
String value concatenation                         | :x:
Array concatenation                                | :x:
Object concatenation                               | :x:
Arrays without commas                              | :x:
Path expressions                                   | :x:
Paths as keys                                      | :x:
Substitutions                                      | :x:
Self-referential substitutions                     | :x:
The `+=` separator                                 | :x:
Includes                                           | :x:
Include semantics: merging                         | :x:
Include semantics: substitution                    | :x:
Include semantics: missing files                   | :x:
Include semantics: file formats and extensions     | :x:
Include semantics: locating resources              | :x:
Include semantics: preventing cycles               | :x:
Conversion of numerically-index objects to arrays  | :x:

