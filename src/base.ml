let finally f finalize =
  let x = try f () with
    | e -> finalize (); raise e
  in x

let with_in path f =
  let chan = open_in path in
  finally
    (fun () -> f chan)
    (fun () -> close_in_noerr chan)

let with_out path f =
  let chan = open_out path in
  finally
    (fun () -> f chan)
    (fun () -> close_out_noerr chan)

let write_all chan content =
  begin
    output_string chan content;
    flush chan;
  end
