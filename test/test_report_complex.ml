open OUnit2
open Trip_tab

(** [money text] converts a valid test amount to money. *)
let money text =
  match Money.of_string text with
  | Some amount -> amount
  | None -> assert_failure ("Invalid test amount: " ^ text)

(** [expense id payer category total shares] constructs a valid expense. *)
let expense id payer category total shares =
  match
    Expense.make ~id ~description:("Expense " ^ string_of_int id) ~payer
      ~category ~total:(money total)
      ~shares:(List.map (fun (person, amount) -> (person, money amount)) shares)
  with
  | Ok value -> value
  | Error message -> assert_failure message

(** [set_category budgets category amount mode] sets a category budget in a
    test fixture. *)
let set_category budgets category amount mode =
  match Budget_complex.set_category budgets category (money amount) mode with
  | Ok updated -> updated
  | Error message -> assert_failure message

(** [set_person budgets person amount mode] sets a person budget in a test
    fixture. *)
let set_person budgets person amount mode =
  match Budget_complex.set_person budgets person (money amount) mode with
  | Ok updated -> updated
  | Error message -> assert_failure message

(** [test_totals _] checks category and person totals independently of the
    person who paid and includes zero totals. *)
let test_totals _ =
  let expenses =
    [
      expense 1 "Amy" Types.Travel "30.00"
        [ ("Amy", "10.00"); ("Sandy", "20.00") ];
      expense 2 "Sandy" Types.Travel "12.50" [ ("Amy", "12.50") ];
      expense 3 "Amy" Types.Dining "18.25"
        [ ("Amy", "8.25"); ("Sandy", "10.00") ];
    ]
  in
  let categories =
    Report_complex.totals_by_category expenses
    |> List.map (fun (category, amount) ->
        (Types.category_to_string category, Money.to_string amount))
  in
  let people =
    Report_complex.totals_by_person [ "Sandy"; "Amy"; "Kaylee" ] expenses
    |> List.map (fun (person, amount) -> (person, Money.to_string amount))
  in
  assert_equal
    [
      ("travel", "42.50");
      ("lodging", "0.00");
      ("dining", "18.25");
      ("shopping", "0.00");
      ("other", "0.00");
    ]
    categories;
  assert_equal
    [ ("Sandy", "30.00"); ("Amy", "30.75"); ("Kaylee", "0.00") ]
    people;
  assert_equal [] (Report_complex.totals_by_person [] expenses)

(** [test_full_report _] checks the complete output for category and person
    budgets with hard, soft, unbudgeted, warning, and overage cases. *)
let test_full_report _ =
  let expenses =
    [
      expense 1 "Amy" Types.Travel "200.00" [ ("Amy", "200.00") ];
      expense 2 "Jennifer" Types.Lodging "170.00"
        [ ("Jennifer", "170.00") ];
      expense 3 "Sandy" Types.Dining "95.00" [ ("Sandy", "95.00") ];
      expense 4 "Kaylee" Types.Shopping "110.00" [ ("Kaylee", "110.00") ];
      expense 5 "Elizabeth" Types.Other "25.00"
        [ ("Elizabeth", "25.00") ];
    ]
  in
  let budgets =
    set_category Budget_complex.empty Types.Travel "250.00"
      Budget_complex.Hard
    |> fun budgets ->
    set_category budgets Types.Lodging "200.00" Budget_complex.Soft
    |> fun budgets ->
    set_category budgets Types.Dining "100.00" Budget_complex.Soft
    |> fun budgets ->
    set_category budgets Types.Shopping "100.00" Budget_complex.Soft
    |> fun budgets ->
    set_person budgets "Amy" "250.00" Budget_complex.Soft
    |> fun budgets ->
    set_person budgets "Jennifer" "170.00" Budget_complex.Hard
    |> fun budgets ->
    set_person budgets "Sandy" "110.00" Budget_complex.Soft
    |> fun budgets -> set_person budgets "Kaylee" "100.00" Budget_complex.Soft
  in
  let actual =
    Report_complex.format_report
      [ "Amy"; "Jennifer"; "Sandy"; "Kaylee"; "Elizabeth" ]
      expenses budgets
  in
  let expected =
    String.concat "\n"
      [
        "TRIP EXPENSE REPORT";
        "===================";
        "";
        "SPENDING BY CATEGORY";
        "-----------------";
        "[Travel]\n  Spent     $200.00\n  Budget    $250.00 (HARD)\n  Status    WITHIN BUDGET - $50.00 left";
        "";
        "[Lodging]\n  Spent     $170.00\n  Budget    $200.00 (SOFT)\n  Status    APPROACHING LIMIT - $30.00 remaining";
        "";
        "[Dining]\n  Spent     $95.00\n  Budget    $100.00 (SOFT)\n  Status    URGENT - only $5.00 remaining";
        "";
        "[Shopping]\n  Spent     $110.00\n  Budget    $100.00 (SOFT)\n  Status    OVERBUDGET by $10.00";
        "";
        "[Other]\n  Spent     $25.00\n  Budget    Not set\n  Status    N/A";
        "";
        "ASSIGNED SHARES BY TRAVELER";
        "---------------------------";
        "Amy ~\n  Share     $200.00\n  Budget    $250.00 (SOFT)\n  Status    WITHIN BUDGET - $50.00 left";
        "";
        "Jennifer ~\n  Share     $170.00\n  Budget    $170.00 (HARD)\n  Status    WITHIN BUDGET - $0.00 left";
        "";
        "Sandy ~\n  Share     $95.00\n  Budget    $110.00 (SOFT)\n  Status    APPROACHING LIMIT - $15.00 remaining";
        "";
        "Kaylee ~\n  Share     $110.00\n  Budget    $100.00 (SOFT)\n  Status    OVERBUDGET by $10.00";
        "";
        "Elizabeth ~\n  Share     $25.00\n  Budget    Not set\n  Status    N/A";
        "";
      ]
  in
  assert_equal ~printer:(fun text -> text) expected actual

(** [test_empty_report _] checks the full report when there are no expenses,
    budgets, or travelers. *)
let test_empty_report _ =
  let actual = Report_complex.format_report [] [] Budget_complex.empty in
  let expected =
    String.concat "\n"
      [
        "TRIP EXPENSE REPORT";
        "===================";
        "";
        "SPENDING BY CATEGORY";
        "-----------------";
        "[Travel]\n  Spent     $0.00\n  Budget    Not set\n  Status    N/A";
        "";
        "[Lodging]\n  Spent     $0.00\n  Budget    Not set\n  Status    N/A";
        "";
        "[Dining]\n  Spent     $0.00\n  Budget    Not set\n  Status    N/A";
        "";
        "[Shopping]\n  Spent     $0.00\n  Budget    Not set\n  Status    N/A";
        "";
        "[Other]\n  Spent     $0.00\n  Budget    Not set\n  Status    N/A";
        "";
        "ASSIGNED SHARES BY TRAVELER";
        "---------------------------";
      ]
  in
  assert_equal ~printer:(fun text -> text) expected actual

(** [test_person_budget_without_expenses _] checks that a configured person
    appears at zero assigned shares and that exact zero is at a hard limit. *)
let test_person_budget_without_expenses _ =
  let budgets =
    set_person Budget_complex.empty "Amy" "0.00" Budget_complex.Hard
  in
  let actual = Report_complex.format_report [ "Amy" ] [] budgets in
  let expected_suffix =
    "Amy ~\n  Share     $0.00\n  Budget    $0.00 (HARD)\n  Status    WITHIN BUDGET - $0.00 left\n"
  in
  let start = String.length actual - String.length expected_suffix in
  assert_bool "report includes zero-share traveler and zero hard budget"
    (start >= 0
    && String.sub actual start (String.length expected_suffix) = expected_suffix)

let suite =
  "Complex report tests"
  >::: [
         "totals" >:: test_totals;
         "complete report" >:: test_full_report;
         "empty report" >:: test_empty_report;
         "zero-share traveler" >:: test_person_budget_without_expenses;
       ]

let () = run_test_tt_main suite
