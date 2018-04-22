open Hocon
    
let empty () = TypeSafeConfig.of_value HoconNull
              
let parse_string doc =
  let lexbuf = Ulexing.from_utf8_string doc in
  let parser = MenhirLib.Convert.Simplified.traditional2revised Parser.document in
  let lexer () = 
    let pos_ref = ref (Ulexer.new_pos "application.conf") in
    let pre_pos = !pos_ref in
    let (post_pos, token) = Ulexer.read !pos_ref lexbuf in
    (token, pre_pos, post_pos)
  in
  parser lexer

