open OUnit2
open Trip_tab

(** [money dollars] parses a valid dollar amount for use in these tests. *)
let money dollars =
  match Money.of_string dollars with
  | Some amount -> amount
  | None -> assert_failure ("Invalid test amount: " ^ dollars)

(** [expense ...] constructs a valid test expense or fails with its reason. *)
let expense ~id ~description ~payer ~category ~total ~shares =
  match Expense.make ~id ~description ~payer ~category ~total ~shares with
  | Ok expense -> expense
  | Error message -> assert_failure ("Invalid test expense: " ^ message)

(** [set budgets category amount] sets a budget or fails the test. *)
let set budgets category amount =
  match Budget.set budgets category amount with
  | Ok updated -> updated
  | Error message -> assert_failure message

(** [test_category_totals _] checks aggregation across multiple expenses and
    confirms that categories with no expenses still appear with zero totals. *)
let test_category_totals _ =
  let expenses =
    [
      expense ~id:1 ~description:"Taxi" ~payer:"Amy" ~category:Types.Travel
        ~total:(money "30.00")
        ~shares:[ ("Amy", money "30.00") ];
      expense ~id:2 ~description:"Train" ~payer:"Sandy" ~category:Types.Travel
        ~total:(money "12.50")
        ~shares:[ ("Sandy", money "12.50") ];
      expense ~id:3 ~description:"Lunch" ~payer:"Amy" ~category:Types.Dining
        ~total:(money "18.25")
        ~shares:[ ("Amy", money "18.25") ];
    ]
  in
  let actual =
    Report.totals_by_category expenses
    |> List.map (fun (category, amount) ->
        (Types.category_to_string category, Money.to_string amount))
  in
  assert_equal
    [
      ("travel", "42.50");
      ("lodging", "0.00");
      ("dining", "18.25");
      ("shopping", "0.00");
      ("other", "0.00");
    ]
    actual;
  assert_equal
    [
      ("travel", "0.00");
      ("lodging", "0.00");
      ("dining", "0.00");
      ("shopping", "0.00");
      ("other", "0.00");
    ]
    (Report.totals_by_category []
    |> List.map (fun (category, amount) ->
        (Types.category_to_string category, Money.to_string amount)))

(** [test_person_totals _] checks that assigned shares are accumulated for each
    requested person, preserve traveler order, and give nonparticipants zero. *)
let test_person_totals _ =
  let expenses =
    [
      expense ~id:1 ~description:"Ride" ~payer:"Amy" ~category:Types.Travel
        ~total:(money "30.00")
        ~shares:[ ("Amy", money "10.00"); ("Jennifer", money "20.00") ];
      expense ~id:2 ~description:"Lunch" ~payer:"Sandy" ~category:Types.Dining
        ~total:(money "12.00")
        ~shares:[ ("Sandy", money "5.00"); ("Amy", money "7.00") ];
    ]
  in
  let actual =
    Report.totals_by_person [ "Amy"; "Jennifer"; "Sandy"; "Kaylee" ] expenses
    |> List.map (fun (person, amount) -> (person, Money.to_string amount))
  in
  assert_equal
    [
      ("Amy", "17.00");
      ("Jennifer", "20.00");
      ("Sandy", "5.00");
      ("Kaylee", "0.00");
    ]
    actual;
  assert_equal [] (Report.totals_by_person [] expenses)

(** [test_report_shows_each_budget_status _] checks that the rendered report
    includes category totals and every budget-status message. *)
let test_report_shows_each_budget_status _ =
  let expenses =
    [
      expense ~id:1 ~description:"Train" ~payer:"Amy" ~category:Types.Travel
        ~total:(money "40.00")
        ~shares:[ ("Amy", money "40.00") ];
      expense ~id:2 ~description:"Hotel" ~payer:"Amy" ~category:Types.Lodging
        ~total:(money "85.00")
        ~shares:[ ("Amy", money "85.00") ];
      expense ~id:3 ~description:"Lunch" ~payer:"Amy" ~category:Types.Dining
        ~total:(money "95.00")
        ~shares:[ ("Amy", money "95.00") ];
      expense ~id:4 ~description:"Souvenirs" ~payer:"Amy"
        ~category:Types.Shopping ~total:(money "101.00")
        ~shares:[ ("Amy", money "101.00") ];
      expense ~id:5 ~description:"Miscellaneous" ~payer:"Amy"
        ~category:Types.Other ~total:(money "20.00")
        ~shares:[ ("Amy", money "20.00") ];
    ]
  in
  let budgets =
    Budget.empty |> fun budgets ->
    set budgets Types.Travel (money "100.00") |> fun budgets ->
    set budgets Types.Lodging (money "100.00") |> fun budgets ->
    set budgets Types.Dining (money "100.00") |> fun budgets ->
    set budgets Types.Shopping (money "100.00")
  in
  let report = Report.format_report [ "Amy" ] expenses budgets in
  let expected =
    String.concat "\n"
      [
        "EXPENSE REPORT";
        "Spending by category: ";
        "| travel: spending ~ $40.00 [WITHIN BUDGET - $60.00 until budget \
         reached]";
        "| lodging: spending ~ $85.00 [FAST APPROACHING - $15.00 until budget \
         reached (15% of budget remaining)]";
        "| dining: spending ~ $95.00 [CUTTING IT CLOSE - $5.00 until budget \
         reached (5% of budget remaining)]";
        "| shopping: spending ~ $101.00 [OVERBUDGET by $1.00]";
        "| other: spending ~ $20.00 [no budget set for category]";
        "";
        "Spending by person: ";
        "| Amy: $341.00";
      ]
  in
  assert_equal expected report

(** [test_empty_report _] checks that an empty trip report still lists all
    categories with zero spending and handles having no travelers. *)
let test_empty_report _ =
  let report = Report.format_report [] [] Budget.empty in
  let expected =
    String.concat "\n"
      [
        "EXPENSE REPORT";
        "Spending by category: ";
        "| travel: spending ~ $0.00 [no budget set for category]";
        "| lodging: spending ~ $0.00 [no budget set for category]";
        "| dining: spending ~ $0.00 [no budget set for category]";
        "| shopping: spending ~ $0.00 [no budget set for category]";
        "| other: spending ~ $0.00 [no budget set for category]";
        "";
        "Spending by person: ";
      ]
  in
  assert_equal expected report

let suite =
  "Report tests"
  >::: [
         "category totals" >:: test_category_totals;
         "person totals" >:: test_person_totals;
         "all budget statuses in report"
         >:: test_report_shows_each_budget_status;
         "empty report" >:: test_empty_report;
       ]

let () = run_test_tt_main suite
