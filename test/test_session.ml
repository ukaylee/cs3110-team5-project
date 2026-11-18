open OUnit2
open Trip_tab

(* HELPERS *)

let make_session () = Session.create (Trip.create ~name:"Vacation")

let expect_ok = function
  | Ok value -> value
  | Error msg -> assert_failure ("Expected Ok, got Error: " ^ msg)

let expect_error = function
  | Error _ -> ()
  | Ok _ -> assert_failure "Expected Error, got Ok"

let add_travelers session people =
  List.fold_left
    (fun session person -> expect_ok (Session.add_traveler session person))
    session people

let make_item name cents participants : Split.item =
  { name; price = Money.of_cents cents; participants }

let make_bill items tax tip : Split.bill =
  { items; tax = Money.of_cents tax; tip = Money.of_cents tip }

let sample_bill () =
  make_bill
    [
      make_item "Pasta" 1800 [ "Amy" ];
      make_item "Nachos" 1200 [ "Amy"; "Sandy"; "Jennifer" ];
    ]
    500 900

let make_expense id description total =
  expect_ok
    (Expense.make ~id ~description ~payer:"Amy" ~category:Types.Lodging
       ~total:(Money.of_cents total)
       ~shares:
         [
           ("Amy", Money.of_cents (total / 2));
           ("Sandy", Money.of_cents (total - (total / 2)));
         ])

let make_expense_session () = add_travelers (make_session ()) [ "Amy"; "Sandy" ]

(* SESSION CREATION *)

let test_create_session _ =
  let session = make_session () in
  assert_equal "Vacation" (Trip.name (Session.trip session))

let test_empty_session _ =
  let session = make_session () in
  assert_equal [] (Trip.travelers (Session.trip session));
  assert_equal [] (Trip.expenses (Session.trip session))

(* TRAVELER TESTS *)

let test_add_traveler _ =
  let session = make_session () in
  let updated = expect_ok (Session.add_traveler session "Amy") in
  assert_bool "Amy should exist" (Trip.is_traveler (Session.trip updated) "Amy")

let test_add_multiple_travelers _ =
  let session =
    add_travelers (make_session ()) [ "Amy"; "Sandy"; "Jennifer" ]
  in
  assert_equal 3 (List.length (Trip.travelers (Session.trip session)))

let test_duplicate_traveler _ =
  let session = add_travelers (make_session ()) [ "Amy" ] in
  expect_error (Session.add_traveler session "Amy")

let test_add_traveler_immutable _ =
  let original = make_session () in
  let updated = expect_ok (Session.add_traveler original "Amy") in
  assert_equal [] (Trip.travelers (Session.trip original));
  assert_equal [ "Amy" ] (Trip.travelers (Session.trip updated))

let test_remove_traveler _ =
  let session = add_travelers (make_session ()) [ "Amy"; "Sandy" ] in
  let updated = expect_ok (Session.remove_traveler session "Amy") in
  assert_bool "Amy should be removed"
    (not (Trip.is_traveler (Session.trip updated) "Amy"));
  assert_bool "Sandy should remain"
    (Trip.is_traveler (Session.trip updated) "Sandy")

let test_remove_unknown_traveler _ =
  let session = make_session () in
  expect_error (Session.remove_traveler session "Unknown")

let test_remove_traveler_immutable _ =
  let original = add_travelers (make_session ()) [ "Amy" ] in
  let updated = expect_ok (Session.remove_traveler original "Amy") in
  assert_bool "Original should still contain Amy"
    (Trip.is_traveler (Session.trip original) "Amy");
  assert_bool "Updated should not contain Amy"
    (not (Trip.is_traveler (Session.trip updated) "Amy"))

(* ITEMIZED BILLS *)

let test_add_bill _ =
  let session =
    add_travelers (make_session ()) [ "Amy"; "Sandy"; "Jennifer" ]
  in
  let updated =
    expect_ok
      (Session.add_bill session "Dinner at Pokebowl" "Amy" Types.Dining
         (sample_bill ()))
  in
  let expenses = Trip.expenses (Session.trip updated) in
  assert_equal 1 (List.length expenses);
  let expense = List.hd expenses in
  assert_equal 4400 (Money.to_cents (Expense.total expense))

let test_add_bill_immutable _ =
  let session =
    add_travelers (make_session ()) [ "Amy"; "Sandy"; "Jennifer" ]
  in
  let updated =
    expect_ok
      (Session.add_bill session "Dinner" "Amy" Types.Dining (sample_bill ()))
  in
  assert_equal 0 (List.length (Trip.expenses (Session.trip session)));
  assert_equal 1 (List.length (Trip.expenses (Session.trip updated)))

let test_add_multiple_bills _ =
  let session =
    add_travelers (make_session ()) [ "Amy"; "Sandy"; "Jennifer" ]
  in
  let session =
    expect_ok
      (Session.add_bill session "Dinner" "Amy" Types.Dining (sample_bill ()))
  in
  let session =
    expect_ok
      (Session.add_bill session "Lunch" "Sandy" Types.Dining
         (make_bill [ make_item "Sandwich" 1000 [ "Amy"; "Sandy" ] ] 0 0))
  in
  let expenses = Trip.expenses (Session.trip session) in
  assert_equal 2 (List.length expenses);
  let ids = List.map Expense.id expenses in
  assert_equal 2 (List.length (List.sort_uniq Int.compare ids))

let test_empty_bill _ =
  let session = add_travelers (make_session ()) [ "Amy" ] in
  expect_error
    (Session.add_bill session "Empty" "Amy" Types.Dining (make_bill [] 0 0))

let test_bill_unknown_payer _ =
  let session = add_travelers (make_session ()) [ "Amy" ] in
  let bill = make_bill [ make_item "Pasta" 1000 [ "Amy" ] ] 0 0 in
  expect_error (Session.add_bill session "Dinner" "Unknown" Types.Dining bill)

let test_bill_unknown_participant _ =
  let session = add_travelers (make_session ()) [ "Amy" ] in
  let bill = make_bill [ make_item "Pasta" 1000 [ "Amy"; "Sandy" ] ] 0 0 in
  expect_error (Session.add_bill session "Dinner" "Amy" Types.Dining bill)

let test_bill_no_participants _ =
  let session = add_travelers (make_session ()) [ "Amy" ] in
  let bill = make_bill [ make_item "Pasta" 1000 [] ] 0 0 in
  expect_error (Session.add_bill session "Dinner" "Amy" Types.Dining bill)

let test_bill_negative_price _ =
  let session = add_travelers (make_session ()) [ "Amy" ] in
  let bill = make_bill [ make_item "Pasta" (-100) [ "Amy" ] ] 0 0 in
  expect_error (Session.add_bill session "Dinner" "Amy" Types.Dining bill)

let test_bill_negative_tax _ =
  let session = add_travelers (make_session ()) [ "Amy" ] in
  let bill = make_bill [ make_item "Pasta" 1000 [ "Amy" ] ] (-100) 0 in
  expect_error (Session.add_bill session "Dinner" "Amy" Types.Dining bill)

let test_bill_negative_tip _ =
  let session = add_travelers (make_session ()) [ "Amy" ] in
  let bill = make_bill [ make_item "Pasta" 1000 [ "Amy" ] ] 0 (-100) in
  expect_error (Session.add_bill session "Dinner" "Amy" Types.Dining bill)

let test_bill_duplicate_participants _ =
  let session = add_travelers (make_session ()) [ "Amy" ] in
  let bill = make_bill [ make_item "Pasta" 1000 [ "Amy"; "Amy" ] ] 0 0 in
  expect_error (Session.add_bill session "Dinner" "Amy" Types.Dining bill)

let test_failed_bill_preserves_trip _ =
  let session = add_travelers (make_session ()) [ "Amy" ] in
  let bill = make_bill [ make_item "Pasta" 1000 [ "Unknown" ] ] 0 0 in
  expect_error (Session.add_bill session "Dinner" "Amy" Types.Dining bill);
  assert_equal [] (Trip.expenses (Session.trip session))

(* EXPENSES *)

let test_delete_unknown_expense _ =
  let session = make_session () in
  expect_error (Session.delete_expense session 999)

let test_delete_expense _ =
  let session =
    add_travelers (make_session ()) [ "Amy"; "Sandy"; "Jennifer" ]
  in
  let session =
    expect_ok
      (Session.add_bill session "Dinner" "Amy" Types.Dining (sample_bill ()))
  in
  let expense = List.hd (Trip.expenses (Session.trip session)) in
  let updated =
    expect_ok (Session.delete_expense session (Expense.id expense))
  in
  assert_equal [] (Trip.expenses (Session.trip updated));
  assert_equal 1 (List.length (Trip.expenses (Session.trip session)))

let test_add_expense _ =
  let session = make_expense_session () in
  let expense = make_expense 1 "Hotel" 10000 in
  let updated = expect_ok (Session.add_expense session expense) in

  assert_equal 1 (List.length (Trip.expenses (Session.trip updated)));
  assert_equal (Some 10000)
    (Option.map
       (fun e -> Money.to_cents (Expense.total e))
       (Trip.find_expense (Session.trip updated) 1))

let test_add_expense_duplicate_id _ =
  let session = make_expense_session () in
  let expense = make_expense 1 "Hotel" 10000 in
  let updated = expect_ok (Session.add_expense session expense) in

  expect_error (Session.add_expense updated expense);
  assert_equal 1 (List.length (Trip.expenses (Session.trip updated)))

let test_add_expense_unknown_traveler _ =
  let session = add_travelers (make_session ()) [ "Amy" ] in
  let expense = make_expense 1 "Hotel" 10000 in

  expect_error (Session.add_expense session expense);
  assert_equal [] (Trip.expenses (Session.trip session))

let test_add_expense_immutable _ =
  let session = make_expense_session () in
  let expense = make_expense 1 "Hotel" 10000 in
  let updated = expect_ok (Session.add_expense session expense) in

  assert_equal [] (Trip.expenses (Session.trip session));
  assert_equal 1 (List.length (Trip.expenses (Session.trip updated)))

let test_edit_expense _ =
  let session = make_expense_session () in
  let original = make_expense 1 "Hotel" 10000 in
  let session = expect_ok (Session.add_expense session original) in

  let edited = make_expense 1 "Updated Hotel" 20000 in
  let updated = expect_ok (Session.edit_expense session edited) in

  match Trip.find_expense (Session.trip updated) 1 with
  | None -> assert_failure "Edited expense should exist"
  | Some expense ->
      assert_equal "Updated Hotel" (Expense.description expense);
      assert_equal 20000 (Money.to_cents (Expense.total expense));
      assert_equal 1 (List.length (Trip.expenses (Session.trip updated)))

let test_edit_nonexistent_expense _ =
  let session = make_expense_session () in
  let expense = make_expense 999 "Hotel" 10000 in

  expect_error (Session.edit_expense session expense);
  assert_equal [] (Trip.expenses (Session.trip session))

let test_edit_expense_immutable _ =
  let session = make_expense_session () in
  let original = make_expense 1 "Hotel" 10000 in
  let session = expect_ok (Session.add_expense session original) in

  let edited = make_expense 1 "Updated Hotel" 20000 in
  let updated = expect_ok (Session.edit_expense session edited) in

  let original_expense =
    Option.get (Trip.find_expense (Session.trip session) 1)
  in
  let updated_expense =
    Option.get (Trip.find_expense (Session.trip updated) 1)
  in

  assert_equal 10000 (Money.to_cents (Expense.total original_expense));
  assert_equal 20000 (Money.to_cents (Expense.total updated_expense))

(* BUDGETS *)

let test_set_budget _ =
  let session = make_session () in
  let updated =
    expect_ok (Session.set_budget session Types.Dining (Money.of_cents 10000))
  in
  let budgets = Trip.budgets (Session.trip updated) in
  assert_equal (Some 10000)
    (Option.map Money.to_cents (List.assoc_opt Types.Dining budgets))

let test_update_budget _ =
  let session = make_session () in
  let session =
    expect_ok (Session.set_budget session Types.Dining (Money.of_cents 10000))
  in
  let updated =
    expect_ok (Session.set_budget session Types.Dining (Money.of_cents 20000))
  in
  let budgets = Trip.budgets (Session.trip updated) in
  assert_equal (Some 20000)
    (Option.map Money.to_cents (List.assoc_opt Types.Dining budgets))

let test_negative_budget _ =
  let session = make_session () in
  expect_error (Session.set_budget session Types.Dining (Money.of_cents (-100)))

let test_budget_immutable _ =
  let original = make_session () in
  let updated =
    expect_ok (Session.set_budget original Types.Dining (Money.of_cents 10000))
  in
  assert_equal [] (Trip.budgets (Session.trip original));
  assert_equal 1 (List.length (Trip.budgets (Session.trip updated)))

(* APPLY *)
let test_apply_add_bill _ =
  let session =
    add_travelers (make_session ()) [ "Amy"; "Sandy"; "Jennifer" ]
  in
  let command =
    Command.AddBill
      {
        description = "Dinner";
        payer = "Amy";
        category = Types.Dining;
        bill = sample_bill ();
      }
  in
  let updated = expect_ok (Session.apply session command) in
  let expenses = Trip.expenses (Session.trip updated) in
  assert_equal 1 (List.length expenses);
  assert_equal 4400 (Money.to_cents (Expense.total (List.hd expenses)))

let test_apply_invalid_bill _ =
  let session = add_travelers (make_session ()) [ "Amy" ] in
  let command =
    Command.AddBill
      {
        description = "Dinner";
        payer = "Amy";
        category = Types.Dining;
        bill = make_bill [] 0 0;
      }
  in
  expect_error (Session.apply session command);
  assert_equal [] (Trip.expenses (Session.trip session))

(* TEST SUITE *)

let suite =
  "Session tests"
  >::: [
         "create session" >:: test_create_session;
         "empty session" >:: test_empty_session;
         "add traveler" >:: test_add_traveler;
         "add multiple travelers" >:: test_add_multiple_travelers;
         "duplicate traveler" >:: test_duplicate_traveler;
         "add traveler immutable" >:: test_add_traveler_immutable;
         "remove traveler" >:: test_remove_traveler;
         "remove unknown traveler" >:: test_remove_unknown_traveler;
         "remove traveler immutable" >:: test_remove_traveler_immutable;
         "add bill" >:: test_add_bill;
         "add bill immutable" >:: test_add_bill_immutable;
         "add multiple bills" >:: test_add_multiple_bills;
         "empty bill" >:: test_empty_bill;
         "bill unknown payer" >:: test_bill_unknown_payer;
         "bill unknown participant" >:: test_bill_unknown_participant;
         "bill no participants" >:: test_bill_no_participants;
         "bill negative price" >:: test_bill_negative_price;
         "bill negative tax" >:: test_bill_negative_tax;
         "bill negative tip" >:: test_bill_negative_tip;
         "bill duplicate participants" >:: test_bill_duplicate_participants;
         "failed bill preserves trip" >:: test_failed_bill_preserves_trip;
         "delete unknown expense" >:: test_delete_unknown_expense;
         "delete expense" >:: test_delete_expense;
         "add expense" >:: test_add_expense;
         "add expense duplicate id" >:: test_add_expense_duplicate_id;
         "add expense unknown traveler" >:: test_add_expense_unknown_traveler;
         "add expense immutable" >:: test_add_expense_immutable;
         "edit expense" >:: test_edit_expense;
         "edit nonexistent expense" >:: test_edit_nonexistent_expense;
         "edit expense immutable" >:: test_edit_expense_immutable;
         "set budget" >:: test_set_budget;
         "update budget" >:: test_update_budget;
         "negative budget" >:: test_negative_budget;
         "budget immutable" >:: test_budget_immutable;
         "apply add bill" >:: test_apply_add_bill;
         "apply invalid bill" >:: test_apply_invalid_bill;
       ]

let () = run_test_tt_main suite
