(** Represents per-category spending limits. *)

type t = (Types.category * Money.t option) list

type status =
  | Unbudgeted
  | Within_budget of Money.t
    (* The amount of money that can be spent before the budget is met exactly *)
  | Preliminary_warning of Money.t
    (* The amount of money left to spend before hitting the budget when 85% of
       the budget has been used up *)
  | Urgent_warning of Money.t
    (* The amount of money left to spend before hitting the budget when 95% of
       the budget has been used up *)
  | Overbudget of
      Money.t (* The amount of money by which spending exceeds the budget *)

(** [empty] is the overall budget when no category has a spending limit. *)
let empty = List.map (fun category -> (category, None)) Types.all_categories

(** [get budgets category] returns the budget for [category] according to
    [budgets], if one exists. Returns None if there is no budget for [category].
*)
let get budgets category =
  match List.assoc_opt category budgets with
  | Some amount -> amount
  | None -> None

(** [set budgets category amount] sets the budget of [category] to [amount] if
    [category] exists in [budgets]. Returns the updated account of budgets for
    every category. Returns Error if [amount] is negative. *)
let set budgets category amount =
  if Money.compare amount Money.zero < 0 then
    Error "Budget must be greater than or equal to zero"
  else
    let updated_budgets =
      List.map
        (fun (current_category, current_amount) ->
          if current_category = category then (category, Some amount)
          else (current_category, current_amount))
        budgets
    in
    Ok updated_budgets

(** [compare_spending budgets category spending] compares the spending in a
    particular category with the budget for that same category, according to
    [budgets]. Returns [Unbudgeted] if there is no budget for [category],
    [Overbudget] if [spending] exceeds the budget for [category],
    [Urgent_warning] if at most 5% of the budget for [category] remains,
    [Preliminary_warning] if at most 15% and above 5% of the budget for
    [category] remains, and [Within_budget] otherwise. Requires
    [spending >= Money.zero]. *)
let compare_spending budgets category spending =
  match get budgets category with
  | None -> Unbudgeted
  | Some budget ->
      if Money.compare spending budget > 0 then
        Overbudget (Money.sub spending budget)
      else if Money.equal Money.zero budget then Within_budget Money.zero
      else
        let remaining = Money.sub budget spending in
        let percent_of_budget_left percent =
          Money.of_cents (Money.to_cents budget * percent / 100)
        in
        if Money.compare remaining (percent_of_budget_left 5) <= 0 then
          Urgent_warning remaining
        else if Money.compare remaining (percent_of_budget_left 15) <= 0 then
          Preliminary_warning remaining
        else Within_budget remaining
