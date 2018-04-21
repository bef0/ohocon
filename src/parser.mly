%{
 module C = TypeSafeConfig
open Hocon
open Lexing

let make_value frags value =
  List.fold_right (fun p acc -> of_tuples [(p, acc)]) frags value
%}

%token TTRUE TFALSE
%token TSEMI TCOLON
%token TBOOL
%token TCOMMA
%token TDOT
%token TDOLLAR
%token TEQ
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

%%
document:
    |     { C.of_value HoconNull }
    | obj { C.of_value $1 }
        
path:
    | TIDENT           { [ $1 ] }
    | path TDOT TIDENT { $1 @ [$3] }

value:
    | TDOLLAR TLBRACE TIDENT TRBRACE  { of_string $3 }
    | TINT                            { of_int $1 }
    | TSTRING                         { of_string $1 }
    | TLBRACKET int_list TRBRACKET    { of_int_list $2 }
    | TLBRACKET string_list TRBRACKET { of_string_list $2 }
    | TLBRACE obj TRBRACE             { $2 }

obj:
    | path TEQ value    { make_value $1 $3 }
    | path TCOLON value { make_value $1 $3 }

int_list:
    | TINT TCOMMA { [$1] }
    | int_list TINT TCOMMA { $1 @ [$2] }

string_list:
    | TSTRING TCOMMA { [$1] }
    | string_list TSTRING TCOMMA { $1 @ [$2] }
%%
