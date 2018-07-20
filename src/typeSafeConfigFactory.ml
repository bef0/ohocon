open Hocon
    
module B = Base

exception FileNotFoundException of string

type path = string

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

let parse_file path =
  let chan   = try open_in path with Sys_error message -> raise (FileNotFoundException message) in
  let n      = in_channel_length chan in
  let buffer = Bytes.create n in
  begin
    B.finally
      (fun () -> really_input chan buffer 0 n)
      (fun () -> close_in_noerr chan);
    parse_string (Bytes.to_string buffer);
  end
