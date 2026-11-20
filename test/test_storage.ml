(** Tests for {!Storage}. Most tests use [to_string] and [of_string] so they
    need no files; a few use a temporary file. Expected texts and values are
    written by hand, not copied from the program's output. Expenses are built
    with [Expense.make] and [Money.of_cents], so these tests do not depend on
    [Money.split_evenly] or [Money.allocate]. *)

open OUnit2
open Trip_tab

(* ---------- helpers ---------- *)

let cents = Money.of_cents

let ok = function
  | Ok x -> x
  | Error msg -> assert_failure ("expected Ok but got Error: " ^ msg)

let assert_error what = function
  | Ok _ -> assert_failure (what ^ ": expected Error but got Ok")
  | Error _ -> ()

let contains s sub =
  let n = String.length s and m = String.length sub in
  let rec go i = i + m <= n && (String.sub s i m = sub || go (i + 1)) in
  go 0

let string_of_strings l = "[" ^ String.concat "; " l ^ "]"

let expense ~id ~payer ~category ~description ~total shares =
  ok
    (Expense.make ~id ~description ~payer ~category ~total:(cents total)
       ~shares:(List.map (fun (p, c) -> (p, cents c)) shares))

let trip_with ?(name = "Weekend") names =
  List.fold_left
    (fun t p -> ok (Trip.add_traveler t p))
    (Trip.create ~name) names

let four = [ "Amy"; "Sandy"; "Jennifer"; "Kaylee" ]

(** [summary t] lists everything {!Storage} is supposed to preserve, in a form
    that differs from the file format so a comparison is not circular. *)
let summary t =
  let share (p, m) = p ^ ":" ^ string_of_int (Money.to_cents m) in
  let expense e =
    Printf.sprintf "#%d %s paid %d [%s] %S: %s" (Expense.id e) (Expense.payer e)
      (Money.to_cents (Expense.total e))
      (Types.category_to_string (Expense.category e))
      (Expense.description e)
      (String.concat " " (List.map share (Expense.shares e)))
  in
  let budget (c, m) = Types.category_to_string c ^ "=" ^ Money.to_string m in
  List.concat
    [
      [ "name " ^ Trip.name t ];
      [ "travelers " ^ String.concat "," (Trip.travelers t) ];
      List.map budget (Trip.budgets t);
      List.map expense (Trip.expenses t);
    ]

let check_same what expected actual =
  assert_equal ~msg:what
    ~printer:(fun l -> String.concat "\n" l)
    (summary expected) (summary actual)

let round_trip t = ok (Storage.of_string (ok (Storage.to_string t)))

(** Amy's scenario 1, plus a dining budget. *)
let scenario1 () =
  let t = trip_with four in
  let t = ok (Trip.set_budget t Types.Dining (cents 7000)) in
  let gas =
    expense ~id:1 ~payer:"Sandy" ~category:Types.Travel
      ~description:"Gas station" ~total:4000
      [ ("Amy", 1000); ("Sandy", 1000); ("Jennifer", 1000); ("Kaylee", 1000) ]
  in
  let dinner =
    expense ~id:2 ~payer:"Amy" ~category:Types.Dining ~description:"Dinner"
      ~total:6000
      [ ("Amy", 2000); ("Sandy", 1500); ("Jennifer", 2500) ]
  in
  let souvenirs =
    expense ~id:3 ~payer:"Jennifer" ~category:Types.Shopping
      ~description:"Souvenirs" ~total:3000
      [ ("Amy", 1500); ("Kaylee", 1500) ]
  in
  let t = ok (Trip.add_expense t gas) in
  let t = ok (Trip.add_expense t dinner) in
  ok (Trip.add_expense t souvenirs)

(* ---------- round trips ---------- *)

let test_round_trip_scenario1 _ =
  let t = scenario1 () in
  let loaded = round_trip t in
  check_same "everything preserved" t loaded;
  (* Hand-checked values, independent of [summary]. *)
  assert_equal ~printer:string_of_strings four (Trip.travelers loaded);
  assert_equal ~printer:string_of_int 4 (Trip.next_id loaded);
  match Trip.find_expense loaded 2 with
  | Some e ->
      assert_equal ~printer:string_of_int 1500
        (Money.to_cents (Expense.share_of e "Sandy"));
      assert_equal ~printer:Fun.id "Dinner" (Expense.description e)
  | None -> assert_failure "expense 2 should load"

let test_round_trip_empty _ =
  let t = Trip.create ~name:"Empty" in
  assert_equal ~printer:Fun.id "triptab 1\ntrip Empty\n"
    (ok (Storage.to_string t));
  let loaded = round_trip t in
  assert_equal ~printer:Fun.id "Empty" (Trip.name loaded);
  assert_equal ~printer:string_of_strings [] (Trip.travelers loaded);
  assert_equal ~printer:string_of_int 1 (Trip.next_id loaded)

let test_round_trip_budgets _ =
  (* Set out of order; the file and the loaded trip follow category order. *)
  let t = trip_with [ "Amy" ] in
  let t = ok (Trip.set_budget t Types.Shopping (cents 3000)) in
  let t = ok (Trip.set_budget t Types.Travel (cents 10000)) in
  let t = ok (Trip.set_budget t Types.Other Money.zero) in
  let loaded = round_trip t in
  check_same "budgets preserved" t loaded;
  assert_equal ~printer:string_of_strings
    [
      "name Weekend";
      "travelers Amy";
      "travel=100.00";
      "shopping=30.00";
      "other=0.00";
    ]
    (summary loaded)

let test_round_trip_expense_order _ =
  (* Edit keeps position; the file must keep that order too. *)
  let t = scenario1 () in
  let edited =
    expense ~id:2 ~payer:"Sandy" ~category:Types.Dining ~description:"Dinner v2"
      ~total:6000
      [ ("Amy", 2000); ("Sandy", 2000); ("Jennifer", 2000) ]
  in
  let t = ok (Trip.edit_expense t edited) in
  let loaded = round_trip t in
  check_same "order and edit preserved" t loaded;
  assert_equal ~printer:string_of_strings [ "1"; "2"; "3" ]
    (List.map (fun e -> string_of_int (Expense.id e)) (Trip.expenses loaded))

let test_round_trip_deleted_newest _ =
  (* Documented limitation: the id counter is not stored, so after deleting the
     newest expense the loaded counter is lower than before. It must still never
     collide with an existing id. *)
  let t = ok (Trip.delete_expense (scenario1 ()) 3) in
  assert_equal ~printer:string_of_int 4 (Trip.next_id t);
  let loaded = round_trip t in
  check_same "data preserved" t loaded;
  assert_equal ~printer:string_of_int 3 (Trip.next_id loaded)

let test_round_trip_awkward_description _ =
  (* ',' and '=' are fine in a description (only '|' and newline are not). *)
  let t = trip_with [ "Amy" ] in
  let e =
    expense ~id:1 ~payer:"Amy" ~category:Types.Other
      ~description:"Lunch, tacos = yum" ~total:1250
      [ ("Amy", 1250) ]
  in
  let t = ok (Trip.add_expense t e) in
  check_same "description preserved" t (round_trip t)

(* ---------- exact text ---------- *)

let mini_text =
  "triptab 1\n\
   trip Mini\n\
   traveler Amy\n\
   traveler Sandy\n\
   budget dining 7000\n\
   expense 1|Sandy|travel|4000|Gas station|Amy=2000,Sandy=2000\n"

let mini () =
  let t = trip_with ~name:"Mini" [ "Amy"; "Sandy" ] in
  let t = ok (Trip.set_budget t Types.Dining (cents 7000)) in
  let e =
    expense ~id:1 ~payer:"Sandy" ~category:Types.Travel
      ~description:"Gas station" ~total:4000
      [ ("Amy", 2000); ("Sandy", 2000) ]
  in
  ok (Trip.add_expense t e)

let test_exact_text_saved _ =
  assert_equal ~printer:Fun.id mini_text (ok (Storage.to_string (mini ())))

let test_exact_text_loaded _ =
  check_same "hand-written file loads" (mini ())
    (ok (Storage.of_string mini_text))

let test_blank_lines_and_crlf _ =
  let with_blanks = "triptab 1\n\ntrip A\n\ntraveler Amy\n\n" in
  assert_equal ~printer:string_of_strings [ "Amy" ]
    (Trip.travelers (ok (Storage.of_string with_blanks)));
  let crlf = "triptab 1\r\ntrip A\r\ntraveler Amy\r\n" in
  assert_equal ~printer:string_of_strings [ "Amy" ]
    (Trip.travelers (ok (Storage.of_string crlf)))

(* ---------- damaged input ---------- *)

let base = "triptab 1\ntrip A\ntraveler Amy\n"

(* (name, file text, pieces the error message must contain) *)
let damaged =
  [
    ("empty string", "", [ "empty" ]);
    ("only blank lines", "  \n\n", [ "empty" ]);
    ("not a trip file", "hello\n", [ "line 1"; "header" ]);
    ("future version", "triptab 2\ntrip A\n", [ "line 1"; "header" ]);
    ("header only", "triptab 1\n", [ "no trip line" ]);
    ( "traveler before trip line",
      "triptab 1\ntraveler Amy\n",
      [ "line 2"; "trip NAME" ] );
    ( "second trip line",
      "triptab 1\ntrip A\ntrip B\n",
      [ "line 3"; "only one trip" ] );
    ( "unknown line type",
      "triptab 1\ntrip A\nhotel Ritz\n",
      [ "line 3"; "unknown line type" ] );
    ( "line numbers count blank lines",
      "triptab 1\n\ntrip A\n\nhotel Ritz\n",
      [ "line 5"; "unknown line type" ] );
    ( "duplicate traveler",
      base ^ "traveler Amy\n",
      [ "line 4"; "already a traveler" ] );
    ( "traveler after an expense",
      base ^ "expense 1|Amy|dining|1000|Lunch|Amy=1000\ntraveler Sandy\n",
      [ "line 5"; "out of order" ] );
    ( "budget amount not a number",
      "triptab 1\ntrip A\nbudget dining abc\n",
      [ "line 3"; "bad budget amount" ] );
    ( "negative budget",
      "triptab 1\ntrip A\nbudget dining -5\n",
      [ "line 3"; "bad budget amount" ] );
    ( "unknown budget category",
      "triptab 1\ntrip A\nbudget food 100\n",
      [ "line 3"; "unknown category" ] );
    ( "wrong number of fields",
      base ^ "expense 1|Amy|dining|1000|Lunch\n",
      [ "line 4"; "expected 6 fields"; "found 5" ] );
    ( "bad expense id",
      base ^ "expense x|Amy|dining|1000|Lunch|Amy=1000\n",
      [ "line 4"; "bad expense id" ] );
    ( "unknown expense category",
      base ^ "expense 1|Amy|food|1000|Lunch|Amy=1000\n",
      [ "line 4"; "unknown category" ] );
    ( "total with a decimal point",
      base ^ "expense 1|Amy|dining|10.50|Lunch|Amy=1000\n",
      [ "line 4"; "bad total" ] );
    ( "share without an equals sign",
      base ^ "expense 1|Amy|dining|1000|Lunch|Amy\n",
      [ "line 4"; "bad share" ] );
    ( "share amount not a number",
      base ^ "expense 1|Amy|dining|1000|Lunch|Amy=lots\n",
      [ "line 4"; "bad share" ] );
    ( "no participants",
      base ^ "expense 1|Amy|dining|1000|Lunch|\n",
      [ "line 4"; "at least one participant" ] );
    ( "shares do not add up to the total",
      base ^ "expense 1|Amy|dining|1000|Lunch|Amy=900\n",
      [ "line 4"; "shares add up" ] );
    ( "payer is not a traveler",
      base ^ "expense 1|Zed|dining|1000|Lunch|Amy=1000\n",
      [ "line 4"; "Zed is not a traveler" ] );
    ( "participant is not a traveler",
      base ^ "expense 1|Amy|dining|1000|Lunch|Amy=500,Zed=500\n",
      [ "line 4"; "Zed is not a traveler" ] );
    ( "duplicate expense id",
      base
      ^ "expense 1|Amy|dining|1000|Lunch|Amy=1000\n\
         expense 1|Amy|dining|500|Snack|Amy=500\n",
      [ "line 5"; "already exists" ] );
    ( "line type with no space",
      "triptab 1\ntrip A\nhotel\n",
      [ "line 3"; "unknown line type" ] );
    ( "budget missing its amount",
      "triptab 1\ntrip A\nbudget dining\n",
      [ "line 3"; "expected: budget CATEGORY CENTS" ] );
    ( "budget with an extra word",
      "triptab 1\ntrip A\nbudget dining 100 extra\n",
      [ "line 3"; "expected: budget CATEGORY CENTS" ] );
  ]

let damaged_test (name, text, pieces) =
  "damaged: " ^ name >:: fun _ ->
  match Storage.of_string text with
  | Ok _ -> assert_failure "expected Error but the text loaded"
  | Error msg ->
      List.iter
        (fun piece ->
          assert_bool
            (Printf.sprintf "error %S should mention %S" msg piece)
            (contains msg piece))
        pieces

(* ---------- text that cannot be saved ---------- *)

let test_unsavable_description _ =
  let t = trip_with [ "Amy" ] in
  let e =
    expense ~id:1 ~payer:"Amy" ~category:Types.Other ~description:"Gas | snacks"
      ~total:100
      [ ("Amy", 100) ]
  in
  assert_error "'|' in a description"
    (Storage.to_string (ok (Trip.add_expense t e)))

let test_unsavable_traveler _ =
  assert_error "'=' in a name" (Storage.to_string (trip_with [ "A=B" ]));
  assert_error "',' in a name" (Storage.to_string (trip_with [ "A,B" ]));
  assert_error "'|' in a name" (Storage.to_string (trip_with [ "A|B" ]))

let test_unsavable_trip_name _ =
  assert_error "newline in the trip name"
    (Storage.to_string (Trip.create ~name:"A\nB"))

(* ---------- files ---------- *)

let temp_path name = Filename.concat (Filename.get_temp_dir_name ()) name

let test_save_and_load_file _ =
  let path = Filename.temp_file "triptab_test" ".trip" in
  Fun.protect
    ~finally:(fun () -> if Sys.file_exists path then Sys.remove path)
    (fun () ->
      let t = scenario1 () in
      ok (Storage.save t path);
      check_same "file round trip" t (ok (Storage.load path)))

let test_load_missing_file _ =
  let path = temp_path "triptab_definitely_missing.trip" in
  if Sys.file_exists path then Sys.remove path;
  assert_error "missing file" (Storage.load path)

let test_save_unwritable_path _ =
  assert_error "directory does not exist"
    (Storage.save (mini ()) "/triptab_no_such_dir_xyz/trip.txt")

let test_save_unsavable_writes_nothing _ =
  let path = temp_path "triptab_should_not_exist.trip" in
  if Sys.file_exists path then Sys.remove path;
  assert_error "unsavable trip" (Storage.save (trip_with [ "A=B" ]) path);
  assert_bool "no file was created" (not (Sys.file_exists path))

(* ---------- suite ---------- *)

let tests =
  "storage tests"
  >::: [
         "round trip: scenario 1" >:: test_round_trip_scenario1;
         "round trip: empty trip" >:: test_round_trip_empty;
         "round trip: budgets" >:: test_round_trip_budgets;
         "round trip: expense order after edit"
         >:: test_round_trip_expense_order;
         "round trip: deleted newest expense" >:: test_round_trip_deleted_newest;
         "round trip: awkward description"
         >:: test_round_trip_awkward_description;
         "exact text: saved" >:: test_exact_text_saved;
         "exact text: loaded" >:: test_exact_text_loaded;
         "blank lines and CRLF" >:: test_blank_lines_and_crlf;
         "unsavable description" >:: test_unsavable_description;
         "unsavable traveler" >:: test_unsavable_traveler;
         "unsavable trip name" >:: test_unsavable_trip_name;
         "save and load a file" >:: test_save_and_load_file;
         "load a missing file" >:: test_load_missing_file;
         "save to an unwritable path" >:: test_save_unwritable_path;
         "unsavable trip writes no file" >:: test_save_unsavable_writes_nothing;
       ]
       @ List.map damaged_test damaged

let () = run_test_tt_main tests
