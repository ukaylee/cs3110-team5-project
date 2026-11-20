(* custom operator to chain steps that can fail *)
let ( let* ) = Result.bind

(* ---------- saving ---------- *)

(* Characters that would make a line ambiguous. *)
let bad_text s = String.contains s '\n' || String.contains s '|'
let bad_name s = bad_text s || String.contains s ',' || String.contains s '='

let check label bad s =
  if bad s then
    Error
      (Printf.sprintf
         "cannot save %s %S: it contains a newline, '|', ',' or '=' that the \
          file format reserves"
         label s)
  else Ok ()

let to_string t =
  let problems =
    List.concat
      [
        [ check "the trip name" bad_text (Trip.name t) ];
        List.map (check "traveler" bad_name) (Trip.travelers t);
        List.map
          (fun e -> check "description" bad_text (Expense.description e))
          (Trip.expenses t);
      ]
  in
  match
    List.find_map
      (function
        | Error m -> Some m
        | Ok () -> None)
      problems
  with
  | Some msg -> Error msg
  | None ->
      let travelers = List.map (fun p -> "traveler " ^ p) (Trip.travelers t) in
      let budgets =
        List.map
          (fun (c, m) ->
            Printf.sprintf "budget %s %d"
              (Types.category_to_string c)
              (Money.to_cents m))
          (Trip.budgets t)
      in
      let expense e =
        let shares =
          Expense.shares e
          |> List.map (fun (p, m) ->
              Printf.sprintf "%s=%d" p (Money.to_cents m))
          |> String.concat ","
        in
        Printf.sprintf "expense %d|%s|%s|%d|%s|%s" (Expense.id e)
          (Expense.payer e)
          (Types.category_to_string (Expense.category e))
          (Money.to_cents (Expense.total e))
          (Expense.description e) shares
      in
      let lines =
        List.concat
          [
            [ "triptab 1"; "trip " ^ Trip.name t ];
            travelers;
            budgets;
            List.map expense (Trip.expenses t);
          ]
      in
      Ok (String.concat "\n" lines ^ "\n")

(* ---------- loading ---------- *)

let is_digit c = c >= '0' && c <= '9'

(* [parse_cents s] is the nonnegative integer written in [s], if [s] is nothing
   but digits. *)
let parse_cents s =
  if s <> "" && String.for_all is_digit s then int_of_string_opt s else None

let rec map_result f = function
  | [] -> Ok []
  | x :: rest ->
      let* y = f x in
      let* ys = map_result f rest in
      Ok (y :: ys)

(* [split_keyword line] is the first word of [line] and everything after the
   first space. *)
let split_keyword line =
  match String.index_opt line ' ' with
  | None -> (line, "")
  | Some i ->
      (String.sub line 0 i, String.sub line (i + 1) (String.length line - i - 1))

let parse_budget trip rest =
  match String.split_on_char ' ' rest with
  | [ c; amount ] ->
      let* category =
        Option.to_result
          ~none:(Printf.sprintf "unknown category %S" c)
          (Types.category_of_string c)
      in
      let* cents =
        Option.to_result
          ~none:(Printf.sprintf "bad budget amount %S" amount)
          (parse_cents amount)
      in
      Trip.set_budget trip category (Money.of_cents cents)
  | _ -> Error "expected: budget CATEGORY CENTS"

let parse_share s =
  match String.split_on_char '=' s with
  | [ person; amount ] -> (
      match parse_cents amount with
      | Some n -> Ok (person, Money.of_cents n)
      | None -> Error (Printf.sprintf "bad share amount %S" amount))
  | _ -> Error (Printf.sprintf "bad share %S (expected person=cents)" s)

let parse_expense trip rest =
  match String.split_on_char '|' rest with
  | [ f_id; payer; f_category; f_total; description; f_shares ] ->
      let* id =
        Option.to_result
          ~none:(Printf.sprintf "bad expense id %S" f_id)
          (parse_cents f_id)
      in
      let* category =
        Option.to_result
          ~none:(Printf.sprintf "unknown category %S" f_category)
          (Types.category_of_string f_category)
      in
      let* total =
        Option.to_result
          ~none:(Printf.sprintf "bad total %S" f_total)
          (parse_cents f_total)
      in
      let* shares =
        if f_shares = "" then Ok []
        else map_result parse_share (String.split_on_char ',' f_shares)
      in
      let* e =
        Expense.make ~id ~description ~payer ~category
          ~total:(Money.of_cents total) ~shares
      in
      Trip.add_expense trip e
  | fields ->
      Error
        (Printf.sprintf "expected 6 fields separated by '|' but found %d"
           (List.length fields))

(* Section order in the file: travelers (0), budgets (1), expenses (2). A line
   may not go back to an earlier section. *)
let parse_line trip phase line =
  let keyword, rest = split_keyword line in
  let in_section n =
    if phase > n then Error "lines are out of order" else Ok ()
  in
  match keyword with
  | "trip" -> Error "only one trip line is allowed, right after the header"
  | "traveler" ->
      let* () = in_section 0 in
      let* trip = Trip.add_traveler trip rest in
      Ok (trip, 0)
  | "budget" ->
      let* () = in_section 1 in
      let* trip = parse_budget trip rest in
      Ok (trip, 1)
  | "expense" ->
      let* trip = parse_expense trip rest in
      Ok (trip, 2)
  | other -> Error (Printf.sprintf "unknown line type %S" other)

let strip_cr l =
  let n = String.length l in
  if n > 0 && l.[n - 1] = '\r' then String.sub l 0 (n - 1) else l

let of_string s =
  let lines =
    String.split_on_char '\n' s
    |> List.mapi (fun i l -> (i + 1, strip_cr l))
    |> List.filter (fun (_, l) -> String.trim l <> "")
  in
  let at n msg = Error (Printf.sprintf "line %d: %s" n msg) in
  let rec go trip phase = function
    | [] -> Ok trip
    | (n, line) :: rest -> (
        match parse_line trip phase line with
        | Error msg -> at n msg
        | Ok (trip, phase) -> go trip phase rest)
  in
  match lines with
  | [] -> Error "the file is empty"
  | (n, header) :: _ when header <> "triptab 1" ->
      at n "expected the header \"triptab 1\""
  | [ _ ] -> Error "the file has a header but no trip line"
  | _ :: (n, trip_line) :: rest -> (
      match split_keyword trip_line with
      | "trip", name -> go (Trip.create ~name) 0 rest
      | _ -> at n "expected a line of the form \"trip NAME\"")

(* ---------- files ---------- *)

let save t path =
  match to_string t with
  | Error msg -> Error msg
  | Ok text -> (
      try
        Out_channel.with_open_text path (fun channel ->
            output_string channel text);
        Ok ()
      with Sys_error msg -> Error msg)

let load path =
  match In_channel.with_open_text path In_channel.input_all with
  | text -> of_string text
  | exception Sys_error msg -> Error msg
