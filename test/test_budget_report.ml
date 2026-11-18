open OUnit2
open Trip_tab

(** [money dollars] converts valid dollar text [dollars] into a [Money.t]
    amount. *)
let money dollars =
  match Money.of_string dollars with
  | Some amount -> amount
  | None -> assert_failure ("Invalid test amount: " ^ dollars)

(** [test_budget_set_and_get _] checks that setting a category budget stores
    that amount as the budget for the correct category and leaves an unset
    category unbudgeted. *)
let test_budget_set_and_get _ =
  let budgets =
    match Budget.set Budget.empty Types.Shopping (money "170.00") with
    | Ok budgets -> budgets
    | Error message -> assert_failure message
  in
  assert_equal (Some "170.00")
    (Option.map Money.to_string (Budget.get budgets Types.Shopping));
  assert_equal None
    (Option.map Money.to_string (Budget.get budgets Types.Travel))

(** [test_negative_budget_rejected _] checks that a negative budget for a
    category is rejected. *)
let test_negative_budget_rejected _ =
  match Budget.set Budget.empty Types.Lodging (Money.of_cents (-1)) with
  | Error _ -> ()
  | Ok _ -> assert_failure "A negative budget must be rejected"

(** [test_spending_statuses _] checks that the unbudgeted, within-budget,
    preliminary warning, urgent warning, and overbudget case are correctly
    accounted for. *)
let test_spending_statuses _ =
  let status_summary = function
    | Budget.Unbudgeted -> "unbudgeted"
    | Budget.Within_budget remaining -> "within $" ^ Money.to_string remaining
    | Budget.Preliminary_warning remaining ->
        "preliminary warning: $" ^ Money.to_string remaining ^ " remaining"
    | Budget.Urgent_warning remaining ->
        "urgent warning: $" ^ Money.to_string remaining ^ " remaining"
    | Budget.Overbudget overage -> "over budget by $" ^ Money.to_string overage
  in
  let budgets =
    match Budget.set Budget.empty Types.Dining (money "100.00") with
    | Ok budgets -> budgets
    | Error message -> assert_failure message
  in
  let status spending =
    Budget.compare_spending budgets Types.Dining (money spending)
    |> status_summary
  in
  assert_equal "unbudgeted"
    (Budget.compare_spending Budget.empty Types.Other (money "500.00")
    |> status_summary);
  assert_equal "within $100.00" (status "0.00");
  assert_equal "preliminary warning: $15.00 remaining" (status "85.00");
  assert_equal "urgent warning: $5.00 remaining" (status "95.00");
  assert_equal "over budget by $1.00" (status "101.00")

(** [make_expense ...] constructs a valid expense. Fails [assert_failure]
    correctly if the supplied expense data violates the invariants of
    [Expense.make]. *)
let make_expense ~id ~description ~payer ~category ~total ~shares =
  match Expense.make ~id ~description ~payer ~category ~total ~shares with
  | Ok expense -> expense
  | Error message -> assert_failure ("Invalid expense: " ^ message)

(** [test_report_person_shares _] checks that Report totals each person's
    assigned shares across expenses and gives nonparticipants a zero total. *)
let test_report_person_shares _ =
  let expense1 =
    make_expense ~id:1 ~description:"Uber" ~payer:"Amy" ~category:Types.Travel
      ~total:(money "30.00")
      ~shares:[ ("Amy", money "10.00"); ("Jennifer", money "20.00") ]
  in
  let expense2 =
    make_expense ~id:2 ~description:"Lunch" ~payer:"Sandy"
      ~category:Types.Dining ~total:(money "12.00")
      ~shares:[ ("Sandy", money "5.00"); ("Amy", money "7.00") ]
  in
  let totals =
    Report.totals_by_person
      [ "Amy"; "Jennifer"; "Sandy"; "Kaylee"; "Elizabeth" ]
      [ expense1; expense2 ]
  in
  assert_equal
    [
      ("Amy", "17.00");
      ("Jennifer", "20.00");
      ("Sandy", "5.00");
      ("Kaylee", "0.00");
      ("Elizabeth", "0.00");
    ]
    (List.map (fun (person, amount) -> (person, Money.to_string amount)) totals)

(** [test_format_report _] checks that the report's category expense totals,
    budget statuses, and per-person assigned shares are correctly represented in
    the formatted report. *)
let test_format_report _ =
  let brunch =
    make_expense ~id:1 ~description:"Brunch" ~payer:"Kaylee"
      ~category:Types.Dining ~total:(money "36.00")
      ~shares:
        [
          ("Kaylee", money "5.00");
          ("Elizabeth", money "8.00");
          ("Jennifer", money "6.00");
          ("Sandy", money "9.00");
          ("Amy", money "8.00");
        ]
  in
  let budgets =
    match Budget.set Budget.empty Types.Dining (money "40.00") with
    | Ok budgets -> budgets
    | Error message -> assert_failure message
  in
  let actual =
    Report.format_report
      [ "Kaylee"; "Elizabeth"; "Jennifer"; "Sandy"; "Amy" ]
      [ brunch ] budgets
  in
  let expected =
    String.concat "\n"
      [
        "EXPENSE REPORT";
        "Spending by category: ";
        "| travel: spending ~ $0.00 [no budget set for category]";
        "| lodging: spending ~ $0.00 [no budget set for category]";
        "| dining: spending ~ $36.00 [FAST APPROACHING - $4.00 until budget \
         reached (15% of budget remaining)]";
        "| shopping: spending ~ $0.00 [no budget set for category]";
        "| other: spending ~ $0.00 [no budget set for category]";
        "";
        "Spending by person: ";
        "| Kaylee: $5.00";
        "| Elizabeth: $8.00";
        "| Jennifer: $6.00";
        "| Sandy: $9.00";
        "| Amy: $8.00";
      ]
  in
  assert_equal expected actual

let suite =
  "budget tests"
  >::: [
         "set and get" >:: test_budget_set_and_get;
         "reject negative" >:: test_negative_budget_rejected;
         "spending statuses" >:: test_spending_statuses;
         "report person shares" >:: test_report_person_shares;
         "format report" >:: test_format_report;
       ]

let () = run_test_tt_main suite
