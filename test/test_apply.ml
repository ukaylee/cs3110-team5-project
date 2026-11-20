open OUnit2
open Trip_tab

(* Fixtures for command-dispatch tests. Kept local so each test executable can
   be built independently without a shared test-support library. *)
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

let make_expense_session () = add_travelers (make_session ()) [ "Amy"; "Sandy" ]

let make_expense id description total =
  expect_ok
    (Expense.make ~id ~description ~payer:"Amy" ~category:Types.Lodging
       ~total:(Money.of_cents total)
       ~shares:
         [
           ("Amy", Money.of_cents (total / 2));
           ("Sandy", Money.of_cents (total - (total / 2)));
         ])

let edit_command id amount split =
  Command.Edit_expense
    {
      id;
      payer = "Amy";
      category = Types.Dining;
      amount;
      split;
      description = "Updated dinner";
    }

(* Command.bill and Split.bill are distinct record types. Keep one canonical
   sample bill and convert it when testing the Command.AddBill constructor. *)
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

let command_bill_of_split (bill : Split.bill) : Command.bill =
  {
    items =
      List.map
        (fun (item : Split.item) ->
          {
            Command.name = item.name;
            price = item.price;
            participants = item.participants;
          })
        bill.items;
    tax = bill.tax;
    tip = bill.tip;
  }

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
        bill = command_bill_of_split (sample_bill ());
      }
  in
  let updated, _ = expect_ok (Session.apply session command) in
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
        bill = command_bill_of_split (make_bill [] 0 0);
      }
  in
  expect_error (Session.apply session command);
  assert_equal [] (Trip.expenses (Session.trip session))

(* APPLY: helpers *)
let apply_ok session command = expect_ok (Session.apply session command)

let assert_contains text substring =
  let text_len = String.length text and sub_len = String.length substring in
  let rec search i =
    if i + sub_len > text_len then false
    else if String.sub text i sub_len = substring then true
    else search (i + 1)
  in
  assert_bool
    (Printf.sprintf "Expected %S to contain %S" text substring)
    (search 0)

let expense_at session id =
  match Trip.find_expense (Session.trip session) id with
  | Some expense -> expense
  | None -> assert_failure (Printf.sprintf "Missing expense #%d" id)

let apply_expense_session () =
  let session = make_expense_session () in
  expect_ok (Session.add_expense session (make_expense 1 "Hotel" 10000))

(* APPLY: travelers *)
let test_apply_add_traveler _ =
  let original = make_session () in
  let updated, message = apply_ok original (Command.Add_traveler "Amy") in
  assert_bool "Amy added" (Trip.is_traveler (Session.trip updated) "Amy");
  assert_equal [] (Trip.travelers (Session.trip original));
  assert_contains message "Amy"

let test_apply_add_duplicate_traveler _ =
  let session = add_travelers (make_session ()) [ "Amy" ] in
  expect_error (Session.apply session (Command.Add_traveler "Amy"))

let test_apply_remove_traveler _ =
  let session = make_expense_session () in
  let updated, message = apply_ok session (Command.Remove_traveler "Amy") in
  assert_bool "Amy removed"
    (not (Trip.is_traveler (Session.trip updated) "Amy"));
  assert_contains message "Amy"

let test_apply_remove_unknown_traveler _ =
  expect_error
    (Session.apply (make_session ()) (Command.Remove_traveler "Unknown"))

(* APPLY: regular expenses *)
let test_apply_add_expense_equal_all _ =
  let session = make_expense_session () in
  let updated, message =
    apply_ok session
      (Command.Add_expense
         ("Amy", Types.Dining, Money.of_cents 1001, Command.Equal_all, "Dinner"))
  in
  let expense = expense_at updated 1 in
  assert_equal 1001 (Money.to_cents (Expense.total expense));
  assert_equal 1001
    (Money.to_cents (Money.sum (List.map snd (Expense.shares expense))));
  assert_equal [ 501; 500 ]
    (List.map
       (fun (_, amount) -> Money.to_cents amount)
       (Expense.shares expense));
  assert_contains message "Dinner"

let test_apply_add_expense_equal_selected _ =
  let session =
    add_travelers (make_session ()) [ "Amy"; "Sandy"; "Jennifer" ]
  in
  let updated, _ =
    apply_ok session
      (Command.Add_expense
         ( "Amy",
           Types.Dining,
           Money.of_cents 1000,
           Command.Equal [ "Amy"; "Jennifer" ],
           "Lunch" ))
  in
  let shares = Expense.shares (expense_at updated 1) in
  assert_equal [ "Amy"; "Jennifer" ] (List.map fst shares);
  assert_equal [ 500; 500 ]
    (List.map (fun (_, amount) -> Money.to_cents amount) shares)

let test_apply_add_expense_exact _ =
  let session = make_expense_session () in
  let updated, _ =
    apply_ok session
      (Command.Add_expense
         ( "Amy",
           Types.Dining,
           Money.of_cents 1000,
           Command.Exact
             [ ("Amy", Money.of_cents 700); ("Sandy", Money.of_cents 300) ],
           "Lunch" ))
  in
  assert_equal 700
    (Money.to_cents (Expense.share_of (expense_at updated 1) "Amy"));
  assert_equal 300
    (Money.to_cents (Expense.share_of (expense_at updated 1) "Sandy"))

let test_apply_add_expense_empty_split _ =
  let session = make_session () in
  expect_error
    (Session.apply session
       (Command.Add_expense
          ("Amy", Types.Dining, Money.of_cents 1000, Command.Equal_all, "Lunch")))

let test_apply_add_expense_negative _ =
  let session = make_expense_session () in
  expect_error
    (Session.apply session
       (Command.Add_expense
          ( "Amy",
            Types.Dining,
            Money.of_cents (-100),
            Command.Equal_all,
            "Lunch" )))

let test_apply_add_expense_invalid_exact _ =
  let session = make_expense_session () in
  expect_error
    (Session.apply session
       (Command.Add_expense
          ( "Amy",
            Types.Dining,
            Money.of_cents 1000,
            Command.Exact [ ("Amy", Money.of_cents 100) ],
            "Lunch" )))

let test_apply_add_expense_unknown_person _ =
  let session = make_expense_session () in
  expect_error
    (Session.apply session
       (Command.Add_expense
          ( "Amy",
            Types.Dining,
            Money.of_cents 1000,
            Command.Equal [ "Amy"; "Unknown" ],
            "Lunch" )))

(* APPLY: edit, delete and list *)
let test_apply_edit_expense _ =
  let session = apply_expense_session () in
  let command = edit_command 1 (Money.of_cents 3000) Command.Equal_all in
  match Session.apply session command with
  | Error msg -> assert_failure msg
  | Ok (updated, _) ->
      let expenses = Trip.expenses (Session.trip updated) in
      assert_equal 1 (List.length expenses);
      let expense = List.hd expenses in
      assert_equal 1 (Expense.id expense);
      assert_equal "Updated dinner" (Expense.description expense);
      assert_equal 3000 (Money.to_cents (Expense.total expense))

let test_apply_edit_nonexistent _ =
  let session = apply_expense_session () in
  let command = edit_command 999 (Money.of_cents 3000) Command.Equal_all in
  match Session.apply session command with
  | Error _ -> ()
  | Ok _ -> assert_failure "Editing nonexistent expense should fail"

let test_apply_edit_invalid_shares _ =
  let session = apply_expense_session () in
  let command =
    edit_command 1 (Money.of_cents 3000)
      (Command.Exact [ ("Amy", Money.of_cents 1000) ])
  in
  match Session.apply session command with
  | Error _ -> ()
  | Ok _ -> assert_failure "Shares should sum to expense total"

let test_apply_delete_expense _ =
  let session = apply_expense_session () in
  let updated, message = apply_ok session (Command.Delete_expense 1) in
  assert_equal [] (Trip.expenses (Session.trip updated));
  assert_equal 1 (List.length (Trip.expenses (Session.trip session)));
  assert_contains message "1"

let test_apply_delete_unknown_expense _ =
  expect_error (Session.apply (make_session ()) (Command.Delete_expense 999))

let test_apply_list_expenses_empty _ =
  let _, message = apply_ok (make_session ()) Command.List_expenses in
  assert_equal "No expenses yet." message

let test_apply_list_expenses _ =
  let session = apply_expense_session () in
  let updated, message = apply_ok session Command.List_expenses in
  assert_contains message "Hotel";
  assert_contains message "Amy";
  assert_equal
    (Trip.expenses (Session.trip session))
    (Trip.expenses (Session.trip updated))

(* APPLY: balances and settlements *)
let test_apply_balances _ =
  let session = apply_expense_session () in
  let _, message = apply_ok session Command.Balances in
  assert_contains message "Amy";
  assert_contains message "Sandy";
  assert_contains message "50.00"

let test_apply_settle _ =
  let session = apply_expense_session () in
  let _, message = apply_ok session Command.Settle in
  assert_contains message "Sandy pays Amy";
  assert_contains message "50.00"

let test_apply_settle_empty _ =
  let _, message = apply_ok (make_session ()) Command.Settle in
  assert_equal "Everyone is settled up." message

(* APPLY: budgets and reports *)
let test_apply_set_budget _ =
  let session = make_session () in
  let updated, message =
    apply_ok session (Command.Set_budget (Types.Dining, Money.of_cents 10000))
  in
  assert_equal (Some 10000)
    (Option.map Money.to_cents
       (Budget.get (Trip.budget_state (Session.trip updated)) Types.Dining));
  assert_contains message "Budget"

let test_apply_set_budget_negative _ =
  expect_error
    (Session.apply (make_session ())
       (Command.Set_budget (Types.Dining, Money.of_cents (-1))))

let test_apply_report _ =
  let session = apply_expense_session () in
  let updated, report = apply_ok session Command.Report in
  assert_contains report "EXPENSE REPORT";
  assert_contains report "Spending by category";
  assert_contains report "Spending by person";
  assert_contains report "Amy";
  assert_equal
    (Trip.expenses (Session.trip session))
    (Trip.expenses (Session.trip updated))

(* APPLY: help and quit *)
let test_apply_help _ =
  let _, message = apply_ok (make_session ()) Command.Help in
  assert_contains message "Available commands";
  assert_contains message "add-expense";
  assert_contains message "quit"

let test_apply_quit _ =
  let session = make_session () in
  let updated, message = apply_ok session Command.Quit in
  assert_equal "Goodbye!" message;
  assert_equal
    (Trip.travelers (Session.trip session))
    (Trip.travelers (Session.trip updated))

(* Suites grouped by command family. *)
let apply_travelers_suite =
  "Apply / Travelers"
  >::: [
         "add traveler" >:: test_apply_add_traveler;
         "duplicate traveler" >:: test_apply_add_duplicate_traveler;
         "remove traveler" >:: test_apply_remove_traveler;
         "remove unknown traveler" >:: test_apply_remove_unknown_traveler;
       ]

let apply_expenses_suite =
  "Apply / Expenses"
  >::: [
         "equal all" >:: test_apply_add_expense_equal_all;
         "equal selected" >:: test_apply_add_expense_equal_selected;
         "exact" >:: test_apply_add_expense_exact;
         "empty split" >:: test_apply_add_expense_empty_split;
         "negative amount" >:: test_apply_add_expense_negative;
         "invalid exact" >:: test_apply_add_expense_invalid_exact;
         "unknown participant" >:: test_apply_add_expense_unknown_person;
         "edit expense" >:: test_apply_edit_expense;
         "edit nonexistent" >:: test_apply_edit_nonexistent;
         "edit invalid shares" >:: test_apply_edit_invalid_shares;
         "delete expense" >:: test_apply_delete_expense;
         "delete unknown expense" >:: test_apply_delete_unknown_expense;
         "list empty" >:: test_apply_list_expenses_empty;
         "list expenses" >:: test_apply_list_expenses;
       ]

let apply_bills_suite =
  "Apply / Bills"
  >::: [
         "add bill" >:: test_apply_add_bill;
         "invalid bill" >:: test_apply_invalid_bill;
       ]

let apply_balances_suite =
  "Apply / Balances and settle"
  >::: [
         "balances" >:: test_apply_balances;
         "settle" >:: test_apply_settle;
         "settle empty" >:: test_apply_settle_empty;
       ]

let apply_budget_report_suite =
  "Apply / Budgets and reports"
  >::: [
         "set budget" >:: test_apply_set_budget;
         "negative budget" >:: test_apply_set_budget_negative;
         "report" >:: test_apply_report;
       ]

let apply_misc_suite =
  "Apply / Help and quit"
  >::: [ "help" >:: test_apply_help; "quit" >:: test_apply_quit ]

let suite =
  "Session.apply tests"
  >::: [
         apply_travelers_suite;
         apply_expenses_suite;
         apply_bills_suite;
         apply_balances_suite;
         apply_budget_report_suite;
         apply_misc_suite;
       ]

let () = run_test_tt_main suite
