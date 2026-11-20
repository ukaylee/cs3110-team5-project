open OUnit2
open Trip_tab

(** [money dollars] parses a valid dollar amount for use in these tests. *)
let money dollars =
  match Money.of_string dollars with
  | Some amount -> amount
  | None -> assert_failure ("Invalid test amount: " ^ dollars)

(** [set budgets category amount] sets a budget or fails the test. *)
let set budgets category amount =
  match Budget.set budgets category amount with
  | Ok updated -> updated
  | Error message -> assert_failure message

(** [status budgets category spending] returns a readable representation of a
    spending status so tests can compare the complete result. *)
let status budgets category spending =
  match Budget.compare_spending budgets category spending with
  | Budget.Unbudgeted -> "unbudgeted"
  | Budget.Within_budget remaining -> "within:" ^ Money.to_string remaining
  | Budget.Preliminary_warning remaining ->
      "preliminary:" ^ Money.to_string remaining
  | Budget.Urgent_warning remaining -> "urgent:" ^ Money.to_string remaining
  | Budget.Overbudget overage -> "over:" ^ Money.to_string overage

(** [test_empty_and_set_get _] checks that an empty budget has no limits and
    that setting one category does not set another. *)
let test_empty_and_set_get _ =
  let budgets = set Budget.empty Types.Shopping (money "170.00") in
  assert_equal (Some "170.00")
    (Option.map Money.to_string (Budget.get budgets Types.Shopping));
  assert_equal None
    (Option.map Money.to_string (Budget.get budgets Types.Travel));
  assert_equal None (Budget.get Budget.empty Types.Dining)

(** [test_replacing_budget _] checks that setting the same category again
    replaces its old limit without changing the original immutable value. *)
let test_replacing_budget _ =
  let original = set Budget.empty Types.Dining (money "100.00") in
  let updated = set original Types.Dining (money "75.00") in
  assert_equal "100.00"
    (Money.to_string (Option.get (Budget.get original Types.Dining)));
  assert_equal "75.00"
    (Money.to_string (Option.get (Budget.get updated Types.Dining)))

(** [test_zero_and_negative_budgets _] checks that zero is allowed, a negative
    limit is rejected, and a rejected update leaves the prior value unchanged.
*)
let test_zero_and_negative_budgets _ =
  let budgets = set Budget.empty Types.Other Money.zero in
  assert_equal "0.00"
    (Money.to_string (Option.get (Budget.get budgets Types.Other)));
  (match Budget.set budgets Types.Other (Money.of_cents (-1)) with
  | Error _ -> ()
  | Ok _ -> assert_failure "A negative budget must be rejected");
  assert_equal "0.00"
    (Money.to_string (Option.get (Budget.get budgets Types.Other)))

(** [test_warning_boundaries _] checks spending just before and at both warning
    thresholds, at the budget limit, and just over it. *)
let test_warning_boundaries _ =
  let budgets = set Budget.empty Types.Dining (money "100.00") in
  let spending amount = money amount in
  assert_equal "within:15.01" (status budgets Types.Dining (spending "84.99"));
  assert_equal "preliminary:15.00"
    (status budgets Types.Dining (spending "85.00"));
  assert_equal "preliminary:5.01"
    (status budgets Types.Dining (spending "94.99"));
  assert_equal "urgent:5.00" (status budgets Types.Dining (spending "95.00"));
  assert_equal "urgent:0.00" (status budgets Types.Dining (spending "100.00"));
  assert_equal "over:0.01" (status budgets Types.Dining (spending "100.01"));
  assert_equal "unbudgeted"
    (status Budget.empty Types.Other (spending "500.00"))

(** [test_zero_budget_status _] checks the status at and above a zero-dollar
    limit, where percentage thresholds do not apply. *)
let test_zero_budget_status _ =
  let budgets = set Budget.empty Types.Lodging Money.zero in
  assert_equal "within:0.00" (status budgets Types.Lodging Money.zero);
  assert_equal "over:0.01" (status budgets Types.Lodging (Money.of_cents 1))

(** [test_independent_category_updates _] checks that repeated updates to one
    category preserve limits and statuses in other categories. *)
let test_independent_category_updates _ =
  let original = Budget.empty in
  let dining = set original Types.Dining (money "100.00") in
  let both = set dining Types.Travel (money "50.00") in
  let updated = set both Types.Dining (money "200.00") in
  assert_equal None (Budget.get original Types.Dining);
  assert_equal "100.00"
    (Money.to_string (Option.get (Budget.get dining Types.Dining)));
  assert_equal "50.00"
    (Money.to_string (Option.get (Budget.get updated Types.Travel)));
  assert_equal "within:150.00"
    (status updated Types.Dining (money "50.00"));
  assert_equal "urgent:0.00"
    (status updated Types.Travel (money "50.00"))

(** [test_cent_thresholds _] checks warning boundaries for a limit whose
    thresholds include fractional cents and must be represented in cents. *)
let test_cent_thresholds _ =
  let budgets = set Budget.empty Types.Other (money "1.01") in
  assert_equal "within:0.16" (status budgets Types.Other (money "0.85"));
  assert_equal "preliminary:0.15"
    (status budgets Types.Other (money "0.86"));
  assert_equal "preliminary:0.06"
    (status budgets Types.Other (money "0.95"));
  assert_equal "urgent:0.05" (status budgets Types.Other (money "0.96"))

(** [test_rejected_update_preserves_all_categories _] checks that rejecting a
    negative update does not disturb another configured category. *)
let test_rejected_update_preserves_all_categories _ =
  let budgets =
    set (set Budget.empty Types.Dining (money "30.00")) Types.Travel
      (money "40.00")
  in
  (match Budget.set budgets Types.Dining (Money.of_cents (-1)) with
  | Error _ -> ()
  | Ok _ -> assert_failure "negative budget accepted");
  assert_equal "30.00"
    (Money.to_string (Option.get (Budget.get budgets Types.Dining)));
  assert_equal "40.00"
    (Money.to_string (Option.get (Budget.get budgets Types.Travel)))

let suite =
  "Budget tests"
  >::: [
         "empty, set, and get" >:: test_empty_and_set_get;
         "replace a category budget" >:: test_replacing_budget;
         "zero and negative budgets" >:: test_zero_and_negative_budgets;
         "warning thresholds" >:: test_warning_boundaries;
         "zero-budget statuses" >:: test_zero_budget_status;
         "independent category updates" >:: test_independent_category_updates;
         "cent-level thresholds" >:: test_cent_thresholds;
         "rejected update preserves categories"
         >:: test_rejected_update_preserves_all_categories;
       ]

let () = run_test_tt_main suite
