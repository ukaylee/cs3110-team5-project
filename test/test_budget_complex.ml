open OUnit2
open Trip_tab

(** [money text] converts a valid test amount to money. *)
let money text =
  match Money.of_string text with
  | Some amount -> amount
  | None -> assert_failure ("Invalid test amount: " ^ text)

(** [expense id payer category total shares] constructs a valid expense for
    budget checks. *)
let expense id payer category total shares =
  match
    Expense.make ~id ~description:("Expense " ^ string_of_int id) ~payer
      ~category ~total:(money total)
      ~shares:(List.map (fun (person, amount) -> (person, money amount)) shares)
  with
  | Ok expense -> expense
  | Error message -> assert_failure message

(** [set_category budgets category amount mode] sets a category limit or fails
    the test with its error. *)
let set_category budgets category amount mode =
  match Budget_complex.set_category budgets category (money amount) mode with
  | Ok updated -> updated
  | Error message -> assert_failure message

(** [set_person budgets person amount mode] sets a person limit or fails the
    test with its error. *)
let set_person budgets person amount mode =
  match Budget_complex.set_person budgets person (money amount) mode with
  | Ok updated -> updated
  | Error message -> assert_failure message

(** [status status] makes all status constructors and their amounts comparable
    in assertions. *)
let status = function
  | Budget_complex.Unbudgeted -> "unbudgeted"
  | Budget_complex.Within_budget amount -> "within:" ^ Money.to_string amount
  | Budget_complex.Preliminary_warning amount ->
      "preliminary:" ^ Money.to_string amount
  | Budget_complex.Urgent_warning amount -> "urgent:" ^ Money.to_string amount
  | Budget_complex.Overbudget amount -> "over:" ^ Money.to_string amount

(** [category_status budgets category spent] summarizes a category result. *)
let category_status budgets category spent =
  Budget_complex.compare_category_spending budgets category (money spent)
  |> status

(** [person_status budgets person spent] summarizes a person result. *)
let person_status budgets person spent =
  Budget_complex.compare_person_spending budgets person (money spent) |> status

(** [assert_valid budgets expenses] expects all hard limits to be satisfied. *)
let assert_valid budgets expenses =
  match Budget_complex.validate_hard_limits budgets expenses with
  | Ok () -> ()
  | Error message -> assert_failure message

(** [assert_rejected budgets expenses] returns the expected hard-limit error. *)
let assert_rejected budgets expenses =
  match Budget_complex.validate_hard_limits budgets expenses with
  | Error message -> message
  | Ok () -> assert_failure "A hard-limit violation was accepted"

(** [test_empty _] checks that an empty budget has no configured limits. *)
let test_empty _ =
  let budgets = Budget_complex.empty in
  assert_equal [] (Budget_complex.category_limits budgets);
  assert_equal [] (Budget_complex.person_limits budgets);
  assert_equal None (Budget_complex.get_category_amount budgets Types.Dining);
  assert_equal None (Budget_complex.get_person_amount budgets "Amy");
  List.iter
    (fun category ->
      assert_equal "unbudgeted" (category_status budgets category "25.00"))
    Types.all_categories;
  assert_equal "unbudgeted" (person_status budgets "Amy" "25.00");
  assert_valid budgets []

(** [test_category_replacement_and_order _] checks category independence,
    replacement, ordering, modes, and the original value's immutability. *)
let test_category_replacement_and_order _ =
  let original = Budget_complex.empty in
  let first = set_category original Types.Shopping "25.00" Budget_complex.Soft in
  let second = set_category first Types.Travel "50.00" Budget_complex.Hard in
  let updated = set_category second Types.Shopping "30.00" Budget_complex.Hard in
  assert_equal None (Budget_complex.get_category_limit original Types.Shopping);
  assert_equal (Some "25.00")
    (Option.map Money.to_string
       (Budget_complex.get_category_amount first Types.Shopping));
  assert_equal (Some "30.00")
    (Option.map Money.to_string
       (Budget_complex.get_category_amount updated Types.Shopping));
  assert_equal Budget_complex.Hard
    (Option.get (Budget_complex.get_category_limit updated Types.Shopping)).mode;
  assert_equal [ Types.Travel; Types.Shopping ]
    (List.map fst (Budget_complex.category_limits updated));
  assert_equal "unbudgeted" (category_status updated Types.Dining "2.00")

(** [test_person_replacement_and_order _] checks trimming, case sensitivity,
    stable insertion order, replacement, and immutability. *)
let test_person_replacement_and_order _ =
  let original = Budget_complex.empty in
  let one = set_person original " Amy " "10.00" Budget_complex.Soft in
  let two = set_person one "Sandy" "20.00" Budget_complex.Hard in
  let three = set_person two "Amy" "15.00" Budget_complex.Hard in
  let four = set_person three "amy" "5.00" Budget_complex.Soft in
  assert_equal [] (Budget_complex.person_limits original);
  assert_equal (Some "10.00")
    (Option.map Money.to_string (Budget_complex.get_person_amount one "Amy"));
  assert_equal (Some "15.00")
    (Option.map Money.to_string
       (Budget_complex.get_person_amount four " Amy "));
  assert_equal Budget_complex.Hard
    (Option.get (Budget_complex.get_person_limit four "Amy")).mode;
  assert_equal [ "Amy"; "Sandy"; "amy" ]
    (List.map fst (Budget_complex.person_limits four))

(** [test_invalid_limits _] checks rejection of negative amounts and blank
    person names without changing earlier budget values. *)
let test_invalid_limits _ =
  let budgets = set_person Budget_complex.empty "Amy" "10.00" Budget_complex.Soft in
  (match
     Budget_complex.set_person budgets "  " (money "5.00") Budget_complex.Hard
   with
  | Error _ -> ()
  | Ok _ -> assert_failure "blank person accepted");
  (match
     Budget_complex.set_person budgets "Amy" (Money.of_cents (-1))
       Budget_complex.Hard
   with
  | Error _ -> ()
  | Ok _ -> assert_failure "negative person limit accepted");
  (match
     Budget_complex.set_category budgets Types.Dining (Money.of_cents (-1))
       Budget_complex.Hard
   with
  | Error _ -> ()
  | Ok _ -> assert_failure "negative category limit accepted");
  assert_equal [ "Amy" ] (List.map fst (Budget_complex.person_limits budgets));
  assert_equal (Some "10.00")
    (Option.map Money.to_string
       (Budget_complex.get_person_amount budgets "Amy"));
  assert_equal None (Budget_complex.get_category_limit budgets Types.Dining)

(** [test_soft_thresholds _] checks both warning boundaries for category and
    person limits, including exact and exceeded budgets. *)
let test_soft_thresholds _ =
  let budgets =
    set_category Budget_complex.empty Types.Dining "100.00"
      Budget_complex.Soft
    |> fun budgets -> set_person budgets "Amy" "100.00" Budget_complex.Soft
  in
  let cases =
    [
      ("84.99", "within:15.01");
      ("85.00", "preliminary:15.00");
      ("94.99", "preliminary:5.01");
      ("95.00", "urgent:5.00");
      ("100.00", "urgent:0.00");
      ("100.01", "over:0.01");
    ]
  in
  List.iter
    (fun (spent, expected) ->
      assert_equal expected (category_status budgets Types.Dining spent);
      assert_equal expected (person_status budgets "Amy" spent))
    cases

(** [test_hard_statuses _] checks that hard limits never issue warnings before
    the limit, while the comparison still identifies a supplied excess. *)
let test_hard_statuses _ =
  let budgets =
    set_category Budget_complex.empty Types.Travel "100.00"
      Budget_complex.Hard
    |> fun budgets -> set_person budgets "Amy" "100.00" Budget_complex.Hard
  in
  List.iter
    (fun (spent, expected) ->
      assert_equal expected (category_status budgets Types.Travel spent);
      assert_equal expected (person_status budgets "Amy" spent))
    [
      ("85.00", "within:15.00");
      ("95.00", "within:5.00");
      ("100.00", "within:0.00");
      ("100.01", "over:0.01");
    ]

(** [test_zero_and_cent_limits _] checks zero limits and cent-level threshold
    rounding for small positive limits. *)
let test_zero_and_cent_limits _ =
  let budgets =
    set_category Budget_complex.empty Types.Other "0.00" Budget_complex.Soft
    |> fun budgets -> set_person budgets "Amy" "0.00" Budget_complex.Hard
    |> fun budgets ->
    set_category budgets Types.Dining "1.01" Budget_complex.Soft
  in
  assert_equal "within:0.00" (category_status budgets Types.Other "0.00");
  assert_equal "over:0.01" (category_status budgets Types.Other "0.01");
  assert_equal "within:0.00" (person_status budgets "Amy" "0.00");
  assert_equal "over:0.01" (person_status budgets "Amy" "0.01");
  assert_equal "within:0.16" (category_status budgets Types.Dining "0.85");
  assert_equal "preliminary:0.15"
    (category_status budgets Types.Dining "0.86");
  assert_equal "urgent:0.05" (category_status budgets Types.Dining "0.96")

(** [test_category_hard_limit _] checks aggregation across expenses, equality
    at the limit, and rejection one cent beyond it. *)
let test_category_hard_limit _ =
  let budgets =
    set_category Budget_complex.empty Types.Dining "30.00"
      Budget_complex.Hard
  in
  let lunch = expense 1 "Amy" Types.Dining "10.00" [ ("Amy", "10.00") ] in
  let dinner = expense 2 "Sandy" Types.Dining "20.00" [ ("Sandy", "20.00") ] in
  let fee = expense 3 "Amy" Types.Dining "0.01" [ ("Amy", "0.01") ] in
  assert_valid budgets [ lunch; dinner ];
  assert_equal
    "[WARNING] Prevention from Exceeding Budget: Hard dining budget set at $30.00, but category spending would be $30.01"
    (assert_rejected budgets [ lunch; dinner; fee ])

(** [test_person_hard_limit_uses_shares _] checks that the payer's full outlay
    is not confused with a person's assigned share. *)
let test_person_hard_limit_uses_shares _ =
  let budgets =
    set_person Budget_complex.empty "Amy" "12.00" Budget_complex.Hard
  in
  let paid_by_amy =
    expense 1 "Amy" Types.Dining "30.00"
      [ ("Amy", "10.00"); ("Sandy", "20.00") ]
  in
  let paid_by_sandy =
    expense 2 "Sandy" Types.Travel "7.00" [ ("Amy", "7.00") ]
  in
  assert_valid budgets [ paid_by_amy ];
  assert_equal
    "[WARNING] Prevention from Exceeding Budget: Hard budget for Amy set at $12.00, but their assigned shares would be $17.00"
    (assert_rejected budgets [ paid_by_amy; paid_by_sandy ])

(** [test_soft_limits_do_not_reject _] checks that soft overages do not prevent
    accepting a proposed collection of expenses. *)
let test_soft_limits_do_not_reject _ =
  let budgets =
    set_category Budget_complex.empty Types.Dining "1.00"
      Budget_complex.Soft
    |> fun budgets -> set_person budgets "Amy" "1.00" Budget_complex.Soft
  in
  let dinner = expense 1 "Amy" Types.Dining "10.00" [ ("Amy", "10.00") ] in
  assert_valid budgets [ dinner ];
  assert_equal "over:9.00" (category_status budgets Types.Dining "10.00");
  assert_equal "over:9.00" (person_status budgets "Amy" "10.00")

(** [test_category_violation_precedes_person _] checks that category rejection
    is reported first when both kinds of hard limit are violated. *)
let test_category_violation_precedes_person _ =
  let budgets =
    set_category Budget_complex.empty Types.Travel "5.00"
      Budget_complex.Hard
    |> fun budgets -> set_person budgets "Amy" "5.00" Budget_complex.Hard
  in
  let taxi = expense 1 "Amy" Types.Travel "10.00" [ ("Amy", "10.00") ] in
  assert_equal
    "[WARNING] Prevention from Exceeding Budget: Hard travel budget set at $5.00, but category spending would be $10.00"
    (assert_rejected budgets [ taxi ])

(** [test_first_person_violation _] checks person-budget rejection order when
    multiple people exceed their hard limits. *)
let test_first_person_violation _ =
  let budgets =
    set_person Budget_complex.empty "Sandy" "1.00" Budget_complex.Hard
    |> fun budgets -> set_person budgets "Amy" "1.00" Budget_complex.Hard
  in
  let dinner =
    expense 1 "Amy" Types.Dining "4.00"
      [ ("Amy", "2.00"); ("Sandy", "2.00") ]
  in
  assert_equal
    "[WARNING] Prevention from Exceeding Budget: Hard budget for Sandy set at $1.00, but their assigned shares would be $2.00"
    (assert_rejected budgets [ dinner ])

(** [test_full_proposed_list _] checks that validation uses the complete
    proposed expense list when an existing expense is replaced. *)
let test_full_proposed_list _ =
  let budgets =
    set_person Budget_complex.empty "Amy" "15.00" Budget_complex.Hard
  in
  let old = expense 1 "Amy" Types.Dining "10.00" [ ("Amy", "10.00") ] in
  let replacement =
    expense 1 "Amy" Types.Dining "12.00" [ ("Amy", "12.00") ]
  in
  let too_large =
    expense 1 "Amy" Types.Dining "12.01" [ ("Amy", "12.01") ]
  in
  let other = expense 2 "Sandy" Types.Travel "3.00" [ ("Amy", "3.00") ] in
  assert_valid budgets [ old; other ];
  assert_valid budgets [ replacement; other ];
  ignore (assert_rejected budgets [ too_large; other ])

let suite =
  "Complex budget tests"
  >::: [
         "empty budgets" >:: test_empty;
         "category replacement and order" >:: test_category_replacement_and_order;
         "person replacement and order" >:: test_person_replacement_and_order;
         "invalid limits" >:: test_invalid_limits;
         "soft warning thresholds" >:: test_soft_thresholds;
         "hard statuses" >:: test_hard_statuses;
         "zero and cent limits" >:: test_zero_and_cent_limits;
         "hard category aggregation" >:: test_category_hard_limit;
         "hard person uses shares" >:: test_person_hard_limit_uses_shares;
         "soft overages accepted" >:: test_soft_limits_do_not_reject;
         "category violation first" >:: test_category_violation_precedes_person;
         "first person violation" >:: test_first_person_violation;
         "complete proposed expense list" >:: test_full_proposed_list;
       ]

let () = run_test_tt_main suite
