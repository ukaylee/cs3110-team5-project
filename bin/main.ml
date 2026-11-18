(** Terminal driver for Trip Tab: starts a trip (new or loaded from a file),
    then reads one command per line until [quit] or end of input.

    This file only talks to the terminal and chooses the starting trip. Parsing
    and running commands belongs to [Command] and [Session]; see [handle]. *)

open Trip_tab

let usage () =
  prerr_endline "Usage: main.exe [--load FILE]";
  exit 1

let interactive = Unix.isatty Unix.stdin

let prompt text =
  if interactive then (
    print_string text;
    flush stdout)

let read_line_opt () = try Some (read_line ()) with End_of_file -> None

(** [initial_trip ()] is the trip to start with: loaded from the file given with
    [--load FILE], or a new trip whose name is read from the terminal. Exits
    with status 1 if the arguments are wrong or the file cannot be loaded. *)
let initial_trip () =
  match List.tl (Array.to_list Sys.argv) with
  | [] ->
      prompt "Name your trip (or press Enter to skip): ";
      let name =
        match read_line_opt () with
        | Some line when String.trim line <> "" -> String.trim line
        | _ -> "Untitled trip"
      in
      Trip.create ~name
  | [ "--load"; path ] -> (
      match Storage.load path with
      | Ok trip -> trip
      | Error msg ->
          Printf.eprintf "Could not load %s: %s\n" path msg;
          exit 1)
  | _ -> usage ()

let describe trip =
  Printf.sprintf "Trip \"%s\": %d traveler(s), %d expense(s)." (Trip.name trip)
    (List.length (Trip.travelers trip))
    (List.length (Trip.expenses trip))

(** [handle trip line] is the new trip and the text to print after running the
    command [line]. TODO: replace the body with [Command.parse] followed by
    [Session.apply] once those modules exist. Until then no command changes the
    trip. *)
let handle trip line =
  (trip, "Commands are not available yet (you typed: " ^ line ^ ")")

let rec loop trip =
  prompt "> ";
  match read_line_opt () with
  | None -> ()
  | Some line -> (
      match String.trim line with
      | "" -> loop trip
      | "quit" | "exit" -> ()
      | command ->
          let trip, output = handle trip command in
          if output <> "" then print_endline output;
          loop trip)

let () =
  let trip = initial_trip () in
  print_endline (describe trip);
  print_endline "Type \"quit\" to leave.";
  loop trip
