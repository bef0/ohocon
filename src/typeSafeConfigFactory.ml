open TypeSafeConfig
module P = Parser

let empty () = []

let of_tuples ts = [HoconObject ts]

let from_string doc =
  let lexbuf = Lexing.from_string doc in
  Parser.document Lexer.read lexbuf
