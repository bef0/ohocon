open Hocon
    
let empty () = TypeSafeConfig.of_value HoconNull
              
let parse_string doc =
  let lexbuf = Lexing.from_string doc in
  Parser.document Lexer.read lexbuf
