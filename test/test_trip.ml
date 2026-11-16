(** Tests for {!Trip}. Expected values are worked out by hand, mostly from the
    team's balance/settle scenario 1 (Amy, Sandy, Jennifer, Kaylee; gas,
    dinner, souvenirs). Expenses are built directly with [Expense.make] and
    [Money.of_cents]. *)

open OUnit2
open Trip_tab

(* ---------- helpers ---------- *)

let cents = Money.of_cents

(** [ok r] is the value inside [Ok]; fails the test on [Error]. *)
let ok = function
  | Ok x -> x
  | Error msg -> assert_failure ("expected Ok but got Error: " ^ msg)

(** [assert_error what r] fails unless [r] is an [Error]. *)
let assert_error what = function
  | Ok _ -> assert_failure (what ^ ": expected Error but got Ok")
  | Error _ -> ()

(** [expense ~id ~payer ~total shares] builds a dining expense; amounts are in
    cents. *)
let expense ~id ~payer ~total shares =
  ok
    (Expense.make ~id ~description:"test" ~payer ~category:Types.Dining
       ~total:(cents total)
       ~shares:(List.map (fun (p, c) -> (p, cents c)) shares))

let trip_with names =
  List.fold_left
    (fun t p -> ok (Trip.add_traveler t p))
    (Trip.create ~name:"Weekend") names

let four = [ "Amy"; "Sandy"; "Jennifer"; "Kaylee" ]
let ids t = List.map Expense.id (Trip.expenses t)
let string_of_ints l = "[" ^ String.concat "; " (List.map string_of_int l) ^ "]"
let string_of_strings l = "[" ^ String.concat "; " l ^ "]"

let budgets_str t =
  List.map
    (fun (c, m) -> Types.category_to_string c ^ "=" ^ Money.to_string m)
    (Trip.budgets t)

let check_ids what expected t =
  assert_equal ~msg:what ~printer:string_of_ints expected (ids t)

let check_people what expected t =
  assert_equal ~msg:what ~printer:string_of_strings expected (Trip.travelers t)

(* Scenario 1 expenses. Payer need not be a participant (souvenirs). *)
let gas () =
  expense ~id:1 ~payer:"Sandy" ~total:4000
    [ ("Amy", 1000); ("Sandy", 1000); ("Jennifer", 1000); ("Kaylee", 1000) ]

let dinner () =
  expense ~id:2 ~payer:"Amy" ~total:6000
    [ ("Amy", 2000); ("Sandy", 1500); ("Jennifer", 2500) ]

let souvenirs () =
  expense ~id:3 ~payer:"Jennifer" ~total:3000 [ ("Amy", 1500); ("Kaylee", 1500) ]

(** [scenario1 ()] is the four travelers with all three expenses added. *)
let scenario1 () =
  let t = trip_with four in
  let t = ok (Trip.add_expense t (gas ())) in
  let t = ok (Trip.add_expense t (dinner ())) in
  ok (Trip.add_expense t (souvenirs ()))

(* ---------- create ---------- *)

let test_create _ =
  let t = Trip.create ~name:"Weekend" in
  assert_equal ~printer:Fun.id "Weekend" (Trip.name t);
  check_people "no travelers" [] t;
  check_ids "no expenses" [] t;
  assert_equal ~printer:string_of_strings [] (budgets_str t);
  assert_equal ~printer:string_of_int 1 (Trip.next_id t)

(* ---------- travelers ---------- *)

let test_add_traveler_order _ =
  let t = trip_with four in
  check_people "insertion order" four t

let test_add_traveler_empty _ =
  let t = trip_with [ "Amy" ] in
  assert_error "empty name" (Trip.add_traveler t "");
  assert_error "spaces only" (Trip.add_traveler t "   ");
  check_people "unchanged after errors" [ "Amy" ] t

let test_add_traveler_duplicate _ =
  let t = trip_with [ "Amy"; "Sandy" ] in
  assert_error "duplicate" (Trip.add_traveler t "Amy");
  check_people "unchanged after error" [ "Amy"; "Sandy" ] t

let test_add_traveler_case_sensitive _ =
  (* Names are exact (Types.person). If the team decides to reject names that
     differ only by case, change this test with the code. *)
  let t = trip_with [ "Amy" ] in
  let t = ok (Trip.add_traveler t "amy") in
  check_people "both spellings" [ "Amy"; "amy" ] t

let test_is_traveler _ =
  let t = trip_with [ "Amy" ] in
  assert_bool "Amy is a traveler" (Trip.is_traveler t "Amy");
  assert_bool "Zed is not" (not (Trip.is_traveler t "Zed"));
  assert_bool "exact match only" (not (Trip.is_traveler t "amy"))

let test_remove_traveler_unknown _ =
  let t = trip_with [ "Amy" ] in
  assert_error "unknown person" (Trip.remove_traveler t "Zed")

let test_remove_traveler_free _ =
  (* Nobody has expenses yet, so removal works and keeps the others' order. *)
  let t = trip_with four in
  let t = ok (Trip.remove_traveler t "Sandy") in
  check_people "Sandy gone, order kept" [ "Amy"; "Jennifer"; "Kaylee" ] t

let test_remove_traveler_in_expense _ =
  let t = scenario1 () in
  (* Jennifer only pays for souvenirs (not in its shares) but is in gas and
     dinner shares; Kaylee is in gas and souvenir shares. Each is involved. *)
  assert_error "payer/participant" (Trip.remove_traveler t "Jennifer");
  assert_error "participant" (Trip.remove_traveler t "Kaylee");
  check_people "unchanged after errors" four t

let test_remove_traveler_payer_only _ =
  (* Zoe pays but is not in the shares; she still cannot be removed. *)
  let t = trip_with [ "Amy"; "Zoe" ] in
  let e = expense ~id:1 ~payer:"Zoe" ~total:1000 [ ("Amy", 1000) ] in
  let t = ok (Trip.add_expense t e) in
  assert_error "payer only" (Trip.remove_traveler t "Zoe");
  (* After deleting the expense, removal succeeds. *)
  let t = ok (Trip.delete_expense t 1) in
  let t = ok (Trip.remove_traveler t "Zoe") in
  check_people "Zoe removed" [ "Amy" ] t

(* ---------- add_expense ---------- *)

let test_add_expense_scenario1 _ =
  let t = scenario1 () in
  check_ids "insertion order" [ 1; 2; 3 ] t;
  assert_equal ~printer:string_of_int 4 (Trip.next_id t)

let test_add_expense_payer_not_participant _ =
  (* Souvenirs: Jennifer pays; her name is not in the shares. *)
  let t = trip_with four in
  let t = ok (Trip.add_expense t (souvenirs ())) in
  check_ids "accepted" [ 3 ] t

let test_add_expense_unknown_payer _ =
  let t = trip_with [ "Amy" ] in
  let e = expense ~id:1 ~payer:"Zed" ~total:1000 [ ("Amy", 1000) ] in
  assert_error "unknown payer" (Trip.add_expense t e)

let test_add_expense_unknown_participant _ =
  let t = trip_with [ "Amy" ] in
  let e =
    expense ~id:1 ~payer:"Amy" ~total:1000 [ ("Amy", 500); ("Zed", 500) ]
  in
  assert_error "unknown participant" (Trip.add_expense t e);
  check_ids "unchanged" [] t

let test_add_expense_duplicate_id _ =
  let t = trip_with four in
  let t = ok (Trip.add_expense t (gas ())) in
  let again =
    expense ~id:1 ~payer:"Amy" ~total:1000 [ ("Amy", 1000) ]
  in
  assert_error "duplicate id" (Trip.add_expense t again);
  check_ids "unchanged" [ 1 ] t

let test_find_expense _ =
  let t = scenario1 () in
  (match Trip.find_expense t 2 with
  | Some e -> assert_equal ~printer:Fun.id "Amy" (Expense.payer e)
  | None -> assert_failure "expense 2 should exist");
  assert_bool "id 99 missing" (Trip.find_expense t 99 = None)

(* ---------- next_id ---------- *)

let test_next_id_not_reused_after_delete _ =
  let t = scenario1 () in
  (* Delete the newest expense: its id (3) must not be handed out again. *)
  let t = ok (Trip.delete_expense t 3) in
  assert_equal ~printer:string_of_int 4 (Trip.next_id t);
  (* Deleting a middle expense does not lower it either. *)
  let t = ok (Trip.delete_expense t 2) in
  assert_equal ~printer:string_of_int 4 (Trip.next_id t)

let test_next_id_after_hand_picked_id _ =
  let t = trip_with [ "Amy" ] in
  let big = expense ~id:10 ~payer:"Amy" ~total:100 [ ("Amy", 100) ] in
  let t = ok (Trip.add_expense t big) in
  assert_equal ~printer:string_of_int 11 (Trip.next_id t);
  (* Adding a smaller id later must not lower the counter. *)
  let small = expense ~id:5 ~payer:"Amy" ~total:100 [ ("Amy", 100) ] in
  let t = ok (Trip.add_expense t small) in
  assert_equal ~printer:string_of_int 11 (Trip.next_id t)

(* ---------- edit_expense ---------- *)

let test_edit_expense_in_place _ =
  let t = scenario1 () in
  (* New dinner: Sandy now paid; three-way even split of $60.00. *)
  let edited =
    expense ~id:2 ~payer:"Sandy" ~total:6000
      [ ("Amy", 2000); ("Sandy", 2000); ("Jennifer", 2000) ]
  in
  let t = ok (Trip.edit_expense t edited) in
  check_ids "same position and count" [ 1; 2; 3 ] t;
  (match Trip.find_expense t 2 with
  | Some e ->
      assert_equal ~printer:Fun.id "Sandy" (Expense.payer e);
      assert_equal ~printer:string_of_int 2000
        (Money.to_cents (Expense.share_of e "Jennifer"))
  | None -> assert_failure "expense 2 should still exist");
  (* Neighbours are untouched. *)
  match Trip.find_expense t 1 with
  | Some e -> assert_equal ~printer:Fun.id "Sandy" (Expense.payer e)
  | None -> assert_failure "expense 1 should still exist"

let test_edit_expense_missing _ =
  let t = trip_with four in
  assert_error "no such id" (Trip.edit_expense t (gas ()))

let test_edit_expense_unknown_person _ =
  let t = scenario1 () in
  let bad = expense ~id:2 ~payer:"Zed" ~total:6000 [ ("Amy", 6000) ] in
  assert_error "unknown payer" (Trip.edit_expense t bad);
  match Trip.find_expense t 2 with
  | Some e -> assert_equal ~printer:Fun.id "Amy" (Expense.payer e)
  | None -> assert_failure "expense 2 should be unchanged"

(* ---------- delete_expense ---------- *)

let test_delete_expense _ =
  let t = scenario1 () in
  let t = ok (Trip.delete_expense t 2) in
  check_ids "others keep their order" [ 1; 3 ] t

let test_delete_expense_missing _ =
  let t = scenario1 () in
  assert_error "no such id" (Trip.delete_expense t 99);
  check_ids "unchanged" [ 1; 2; 3 ] t

let test_old_value_unchanged _ =
  (* Immutability: a successful change must not affect the old trip. *)
  let t = scenario1 () in
  let _ = ok (Trip.delete_expense t 1) in
  check_ids "old trip still has all three" [ 1; 2; 3 ] t

(* ---------- budgets ---------- *)

let test_budgets_optional _ =
  (* A trip with no budgets set is perfectly valid and reports none. *)
  let t = scenario1 () in
  assert_equal ~printer:string_of_strings [] (budgets_str t)

let test_set_budget _ =
  let t = trip_with [ "Amy" ] in
  let t = ok (Trip.set_budget t Types.Dining (cents 7000)) in
  assert_equal ~printer:string_of_strings [ "dining=70.00" ] (budgets_str t)

let test_set_budget_replaces _ =
  let t = trip_with [ "Amy" ] in
  let t = ok (Trip.set_budget t Types.Dining (cents 7000)) in
  let t = ok (Trip.set_budget t Types.Dining (cents 5000)) in
  assert_equal ~printer:string_of_strings [ "dining=50.00" ] (budgets_str t)

let test_set_budget_order _ =
  (* Set Shopping before Travel; output follows Types.all_categories. *)
  let t = trip_with [ "Amy" ] in
  let t = ok (Trip.set_budget t Types.Shopping (cents 3000)) in
  let t = ok (Trip.set_budget t Types.Travel (cents 10000)) in
  assert_equal ~printer:string_of_strings
    [ "travel=100.00"; "shopping=30.00" ]
    (budgets_str t)

let test_set_budget_zero_and_negative _ =
  let t = trip_with [ "Amy" ] in
  let t = ok (Trip.set_budget t Types.Other Money.zero) in
  assert_equal ~printer:string_of_strings [ "other=0.00" ] (budgets_str t);
  assert_error "negative" (Trip.set_budget t Types.Other (cents (-1)));
  assert_equal ~printer:string_of_strings [ "other=0.00" ] (budgets_str t)

(* ---------- suite ---------- *)

let tests =
  "trip tests"
  >::: [
         "create" >:: test_create;
         "add_traveler keeps order" >:: test_add_traveler_order;
         "add_traveler rejects empty" >:: test_add_traveler_empty;
         "add_traveler rejects duplicate" >:: test_add_traveler_duplicate;
         "add_traveler is case-sensitive" >:: test_add_traveler_case_sensitive;
         "is_traveler" >:: test_is_traveler;
         "remove_traveler unknown" >:: test_remove_traveler_unknown;
         "remove_traveler with no expenses" >:: test_remove_traveler_free;
         "remove_traveler in an expense" >:: test_remove_traveler_in_expense;
         "remove_traveler payer only" >:: test_remove_traveler_payer_only;
         "add_expense scenario 1" >:: test_add_expense_scenario1;
         "add_expense payer not a participant"
         >:: test_add_expense_payer_not_participant;
         "add_expense unknown payer" >:: test_add_expense_unknown_payer;
         "add_expense unknown participant"
         >:: test_add_expense_unknown_participant;
         "add_expense duplicate id" >:: test_add_expense_duplicate_id;
         "find_expense" >:: test_find_expense;
         "next_id not reused after delete"
         >:: test_next_id_not_reused_after_delete;
         "next_id after hand-picked id" >:: test_next_id_after_hand_picked_id;
         "edit_expense in place" >:: test_edit_expense_in_place;
         "edit_expense missing id" >:: test_edit_expense_missing;
         "edit_expense unknown person" >:: test_edit_expense_unknown_person;
         "delete_expense" >:: test_delete_expense;
         "delete_expense missing id" >:: test_delete_expense_missing;
         "old trip unchanged" >:: test_old_value_unchanged;
         "budgets are optional" >:: test_budgets_optional;
         "set_budget" >:: test_set_budget;
         "set_budget replaces" >:: test_set_budget_replaces;
         "set_budget category order" >:: test_set_budget_order;
         "set_budget zero and negative" >:: test_set_budget_zero_and_negative;
       ]

let () = run_test_tt_main tests