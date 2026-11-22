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

(** [read_line_opt ()] reads a line from standard input, or returns [None] at
    end of input. *)
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

(** [describe trip] returns a brief description of [trip] and its current
    numbers of travelers and expenses. *)
let describe trip =
  Printf.sprintf "Trip \"%s\": %d traveler(s), %d expense(s)." (Trip.name trip)
    (List.length (Trip.travelers trip))
    (List.length (Trip.expenses trip))

(** [handle session line] parses and executes [line]. It returns the updated
    session, text to display, and whether the command ended the session. On an
    error, it returns the original session and an error message. *)
let handle session line =
  match Command.parse line with
  | Error msg -> (session, "Error: " ^ msg, false)
  | Ok command -> (
      match Session.apply session command with
      | Error msg -> (session, "Error: " ^ msg, false)
      | Ok (updated_session, output) ->
          (updated_session, output, Command.is_quit command))

(** [loop session] reads and executes commands until the user quits or input
    ends. Each successful command passes its updated session to the next turn.
*)
let rec loop session =
  prompt "> ";
  match read_line_opt () with
  | None -> ()
  | Some line ->
      let line = String.trim line in
      if line = "" then loop session
      else
        let updated_session, output, should_quit = handle session line in
        if output <> "" then print_endline output;
        if not should_quit then loop updated_session

(** Starts the terminal session for a new or loaded trip. *)
let () =
  let trip = initial_trip () in
  let session = Session.create trip in
  print_endline (describe trip);
  print_endline "Type \"help\" for commands or \"quit\" to leave.";
  loop session
