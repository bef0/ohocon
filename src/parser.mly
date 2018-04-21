%{
open TypeSafeConfig
open Lexing

let make_value frags value =
  List.fold_right (fun p acc -> HoconObject([(p, acc)])) frags value
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
    |     { [] }
    | obj { [$1] }
        
path:
    | TIDENT           { [ $1 ] }
    | path TDOT TIDENT { $1 @ [$3] }

value:
    | TDOLLAR TLBRACE TIDENT TRBRACE  { HoconString $3 }
    | TINT                            { HoconInt $1 }
    | TSTRING                         { HoconString $1 }
    | TLBRACKET int_list TRBRACKET    { HoconIntList $2 }
    | TLBRACKET string_list TRBRACKET { HoconStringList $2 }
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
