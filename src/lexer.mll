{
open Parser
open Lexing
exception SyntaxError of string
let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}

let white = [' ' '\t']
let newline = '\r' | '\n' | "\r\n" | '\t'
let digit = ['0'-'9']
let ascii = ['a'-'z' 'A'-'Z' '0'-'9']
let integer = '-'? digit digit*
let ident = ascii+
let nano   = "ns" | "nano" | "nanos" | "nanosecond" | "nanoseconds"
let milli  = "ms" | "milli" | "millis" | "millisecond" | "milliseconds"
let second = "s" | "second" | "seconds"
let minute = "m" | "minute" | "minutes"
let hour   = "h" | "hour" | "hours"
let day    = "d" | "day" | "days"

rule read = parse
    | white             { read lexbuf }
    | newline           { next_line lexbuf; read lexbuf }
    | integer           { TINT (int_of_string(Lexing.lexeme lexbuf)) }
    | "true"            { TTRUE }
    | "false"           { TFALSE }
    | "null"            { TNULL }
    | '.'               { TDOT }
    | ','               { TCOMMA }
    | '"'               { read_string (Buffer.create 17) lexbuf }
    | "="               { TEQ }
    | '{'               { TLBRACE }
    | '}'               { TRBRACE }
    | '['               { TLBRACKET }
    | ']'               { TRBRACKET }
    | '('               { TLPAREN }
    | ')'               { TRPAREN }
    | ':'               { TCOLON }
    | '$'               { TDOLLAR }
    | '?'               { TQM }
    | nano              { TNANO }
    | milli             { TMILLI }
    | second            { TSECOND }
    | minute            { TMINUTE }
    | hour              { THOUR }
    | day               { TDAY }
    | ident             { TIDENT (Lexing.lexeme lexbuf) }
    | eof               { TEOF }
    | _
    { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
and read_string buf =
    parse
    | '"'
    { TSTRING (Buffer.contents buf) }
    | '\\' '/'
    { Buffer.add_char buf '/'; read_string buf lexbuf }
    | '\\' '\\'
    { Buffer.add_char buf '\\'; read_string buf lexbuf }
    | '\\' 'n'
    { Buffer.add_char buf '\n'; read_string buf lexbuf }
    | '\\' 't'
    { Buffer.add_char buf '\t'; read_string buf lexbuf }
    | [^ '"' '\\']
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
    | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme
    lexbuf)) }
    | eof { raise (SyntaxError("String is not terminated")) }

{
}
