%{
open Hocon

let fixup_path frags = List.filter (fun e -> e <> "") frags

let make_value frags value =
  let frags' = fixup_path frags in
  List.fold_right (fun p acc -> of_tuples [(p, acc)]) frags' value

let merge_values a =
  match a with
  | (HoconObject(es1), HoconObject(es2)) -> HoconObject(List.append es1 es2)
  | _ -> failwith "bug in merge_values"

%}

%token TSEMI TCOLON
%token <bool> TBOOL
%token <float> TFLOAT
%token TCOMMA
%token TDOT
%token TDOLLAR
%token TEQ
%token TQM
%token TNULL
%token <Duration.t> TNANO TMILLI TSECOND TMINUTE THOUR TDAY
%token TLPAREN TRPAREN
%token TLBRACE TRBRACE
%token TLBRACKET TRBRACKET
%token <string> TSTRING
%token <int> TINT
%token <string> TIDENT
%token TEOF

%start <TypeSafeConfig.t> document

%%
document:
    | (* empty *) { TypeSafeConfig.of_value HoconNull }
    | v = value   { TypeSafeConfig.of_value v }
    ;
        
path:
    | p = TIDENT                  { fixup_path [ p ] }
    | ps = path; TDOT; p = TIDENT { fixup_path (ps @ [p]) }
    ;

path_expr:
    | ps = path_expr1       { of_path_expr (ps, false) }
    | TQM; ps = path_expr1  { of_path_expr (ps, true) }
    ;

path_expr1:
    | p = TIDENT                         { fixup_path [ p ] }
    | ps = path_expr1; TDOT; p = TIDENT; { fixup_path (ps @ [p]) }
    ;

value:
    | TDOLLAR; TLBRACE; v = path_expr; TRBRACE { v }
    | v = duration                             { of_duration v }
    | v = TBOOL                                { of_bool v }
    | v = TINT                                 { of_int v }
    | v = TFLOAT                               { of_float v }
    | v = TSTRING                              { of_string v }
    | TLBRACKET; v = element_list; TRBRACKET   { of_list v }
    | v = pair                                 { v }
    | TLBRACE; v = obj; TRBRACE                { v }
    | TNULL                                    { null }
    ;

obj:
    | p = pair                    { p }
    | ps = obj; TCOMMA; p = pair  { merge_values (p, ps) }
    ;

pair:
    | p = path; TEQ; v = value      { make_value p v }
    | p = path; TCOLON; v = value   { make_value p v }
    ;

element_list:
    | v = value; TCOMMA { [v] }
    | vs = element_list; v = value; TCOMMA { vs @ [v] }
    ;

duration:
    | v = TNANO    { v }
    | v = TMILLI   { v }
    | v = TSECOND  { v }
    | v = TMINUTE  { v }
    | v = THOUR    { v }
    | v = TDAY     { v }
    ;
