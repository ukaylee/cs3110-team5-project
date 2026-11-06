(** Responsible for command-line arguments, terminal communication, and exit
    status. *)

let () =
  if Array.length Sys.argv = 3 then (
    let input_filename = Sys.argv.(1) in
    let output_filename = Sys.argv.(2) in
    let svg = A1.Generator.generate input_filename output_filename in
    match svg with
    | Ok _ -> ()
    | Error message ->
        Printf.eprintf "%s\n" message;
        exit 1)
  else (
    Printf.eprintf
      "Expected exactly two arguments\nUsage: %s <input.pic> <output.svg>\n"
      Sys.argv.(0);
    exit 1)
