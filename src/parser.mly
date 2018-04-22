%{
module C = TypeSafeConfig
open Hocon
open Lexing

let fixup_path frags = List.filter (fun e -> e <> "") frags

let make_value frags value =
  let frags' = fixup_path frags in
  List.fold_right (fun p acc -> of_tuples [(p, acc)]) frags' value

let merge_values a =
  match a with
  | (HoconObject(es1), HoconObject(es2)) -> HoconObject(List.append es1 es2)
  | _ -> failwith "bug in merge_values"

%}

%token TTRUE TFALSE
%token TSEMI TCOLON
%token TBOOL
%token TCOMMA
%token TDOT
%token TDOLLAR
%token TEQ
%token TQM
%token TNULL
%token TNANO TMILLI TSECOND TMINUTE THOUR TDAY
%token TLPAREN TRPAREN
%token TLBRACE TRBRACE
%token TLBRACKET TRBRACKET
%token <string> TSTRING
%token <int> TINT
%token <string> TIDENT
%token TEOF

%start document
%type <TypeSafeConfig.t> document

%nonassoc TSEMI
%left TCOMMA
%left TCOLON
%nonassoc TEQ
%left TDOT
%left prec_app
%nonassoc TQM

%%
document:
    |       { C.of_value HoconNull }
    | value { C.of_value $1 }
        
path:
    | TIDENT           { fixup_path [ $1 ] }
    | path TDOT TIDENT { fixup_path ($1 @ [$3]) }

path_expr:
    | path_expr1       { of_path_expr ($1, false) }
    | TQM path_expr1   { of_path_expr ($2, true) }

path_expr1:
    | TIDENT                 { fixup_path [ $1 ] }
    | path_expr1 TDOT TIDENT { fixup_path ($1 @ [$3]) }

value:
    | TDOLLAR TLBRACE path_expr TRBRACE { $3 }
    | TINT                              { of_int $1 }
    | TSTRING                           { of_string $1 }
    | TLBRACKET int_list TRBRACKET      { of_int_list $2 }
    | TLBRACKET string_list TRBRACKET   { of_string_list $2 }
    | pair                              { $1 }
    | TLBRACE obj TRBRACE               { $2 }
    | TNULL                             { null }                    

obj:
    | pair                    { $1 }
    | obj  TCOMMA pair        { merge_values ($1, $3) }

pair:
    | path TEQ value                { make_value $1 $3 }
    | path TCOLON value             { make_value $1 $3 }

int_list:
    | TINT TCOMMA { [$1] }
    | int_list TINT TCOMMA { $1 @ [$2] }

string_list:
    | TSTRING TCOMMA { [$1] }
    | string_list TSTRING TCOMMA { $1 @ [$2] }
%%
