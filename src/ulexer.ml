open Parser
open Hocon
open Stdint

module U = Ulexing

exception SyntaxError of string

let regexp white = [' ' '\t']
let regexp newline = '\r' | '\n' | "\r\n" | '\t'
let regexp digit = ['0'-'9']
let regexp digits = digit+
let regexp digit1to9 = ['1'-'9']
let regexp ascii = ['a'-'z' 'A'-'Z' '0'-'9']
let regexp integer = '-'? digit | '-'? digit1to9 digits
let regexp frac = '.' digits
let regexp exp = ['e' 'E'] ['+' '-']? digits
let regexp float = integer frac | integer exp | integer frac exp
let regexp ident = ascii+
let regexp duration = digit1to9 digit*
let regexp nano   = duration ("ns" | "nano" | "nanos" | "nanosecond" | "nanoseconds")
let regexp milli  = duration ("ms" | "milli" | "millis" | "millisecond" | "milliseconds")
let regexp second = duration ("s" | "second" | "seconds")
let regexp minute = duration ("m" | "minute" | "minutes")
let regexp hour   = duration ("h" | "hour" | "hours")
let regexp day    = duration ("d" | "day" | "days")

let take_while_digit str =
  let rec find_non_digit_pos n m =
    if m > 0 then
      match String.get str n with
      | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ->
        find_non_digit_pos (1 + n) (m - 1)
      | _ -> n
    else n
  in
  let pos = find_non_digit_pos 0 (String.length str) in
  String.sub str 0 pos

let parse_duration str dunit =
  let digits = take_while_digit str in
  Duration.create (Uint64.of_string digits) dunit
    
let new_pos fname = {
  Lexing.pos_fname = fname;
  Lexing.pos_lnum  = 1;
  Lexing.pos_bol   = 0;
  Lexing.pos_cnum  = 0;
}

let eat pos n =
  let open Lexing in
  {
    pos with pos_cnum = pos.pos_cnum + n;
  }

let next_line pos =
  let open Lexing in
  {
    pos with pos_lnum = pos.pos_lnum + 1;
  }

let rec read pos =
  lexer
  | white   -> read (eat pos 1) lexbuf
  | newline -> read (next_line pos) lexbuf
  | integer -> eat pos (Ulexing.lexeme_length lexbuf), TINT (int_of_string(Ulexing.utf8_lexeme lexbuf))
  | float   -> eat pos (Ulexing.lexeme_length lexbuf), TFLOAT (float_of_string (Ulexing.utf8_lexeme lexbuf))
  | "true"  -> eat pos (Ulexing.lexeme_length lexbuf), TBOOL true
  | "false" -> eat pos (Ulexing.lexeme_length lexbuf), TBOOL false
  | "null"  -> eat pos (Ulexing.lexeme_length lexbuf), TNULL
  | '.'     -> eat pos (Ulexing.lexeme_length lexbuf), TDOT
  | ','     -> eat pos (Ulexing.lexeme_length lexbuf), TCOMMA
  | '"'     -> read_string "" (eat pos 1) lexbuf
  | "="     -> eat pos (Ulexing.lexeme_length lexbuf), TEQ
  | '{'     -> eat pos (Ulexing.lexeme_length lexbuf), TLBRACE
  | '}'     -> eat pos (Ulexing.lexeme_length lexbuf), TRBRACE
  | '['     -> eat pos (Ulexing.lexeme_length lexbuf), TLBRACKET
  | ']'     -> eat pos (Ulexing.lexeme_length lexbuf), TRBRACKET
  | '('     -> eat pos (Ulexing.lexeme_length lexbuf), TLPAREN
  | ')'     -> eat pos (Ulexing.lexeme_length lexbuf), TRPAREN
  | ':'     -> eat pos (Ulexing.lexeme_length lexbuf), TCOLON
  | '$'     -> eat pos (Ulexing.lexeme_length lexbuf), TDOLLAR
  | '?'     -> eat pos (Ulexing.lexeme_length lexbuf), TQM
  | nano    -> eat pos (Ulexing.lexeme_length lexbuf), TNANO (parse_duration (Ulexing.utf8_lexeme lexbuf) Duration.Nano)
  | milli   -> eat pos (Ulexing.lexeme_length lexbuf), TMILLI (parse_duration (Ulexing.utf8_lexeme lexbuf) Duration.Milli)
  | second  -> eat pos (Ulexing.lexeme_length lexbuf), TSECOND (parse_duration (Ulexing.utf8_lexeme lexbuf) Duration.Second)
  | minute  -> eat pos (Ulexing.lexeme_length lexbuf), TMINUTE (parse_duration (Ulexing.utf8_lexeme lexbuf) Duration.Minute)
  | hour    -> eat pos (Ulexing.lexeme_length lexbuf), THOUR (parse_duration (Ulexing.utf8_lexeme lexbuf) Duration.Hour)
  | day     -> eat pos (Ulexing.lexeme_length lexbuf), TDAY (parse_duration (Ulexing.utf8_lexeme lexbuf) Duration.Day)
  | ident   -> eat pos (Ulexing.lexeme_length lexbuf), TIDENT (Ulexing.utf8_lexeme lexbuf)
  | eof     -> eat pos (Ulexing.lexeme_length lexbuf), TEOF
  | _       -> raise (SyntaxError ("Unexpected char: " ^ Ulexing.utf8_lexeme lexbuf))
and read_string buf pos =
  lexer
  | '"'          -> eat pos 1, TSTRING buf
  | [^ '"' '\\'] -> let s = U.utf8_lexeme lexbuf and p = eat pos 1 in read_string (buf ^ s) p lexbuf
  | eof          -> raise (SyntaxError("String is not terminated"))
  | _            -> raise (SyntaxError ("Illegal string character: " ^ (U.utf8_lexeme lexbuf)))
