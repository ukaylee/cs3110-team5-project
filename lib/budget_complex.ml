(** Represents per-category and per-person spending limits. *)

type mode =
  | Soft
  | Hard

type limit = {
  amount : Money.t;
  mode : mode;
}

type t = {
  categories : (Types.category * limit option) list;
  people : (Types.person * limit) list;
}

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

(** [empty] is the overall budget when no category or person has a spending
    limit. *)
let empty =
  {
    categories =
      List.map (fun category -> (category, None)) Types.all_categories;
    people = [];
  }

(** [get_category_limit budgets category] returns the complete budget setting
    details for [category], specifically its amount and mode, if one exists. *)
let get_category_limit budgets category =
  match List.assoc_opt category budgets.categories with
  | Some limit -> limit
  | None -> None

(** [get_person_limit budgets person] returns the complete budget setting
    details for [person], specifically its amount and mode, if one exists. *)
let get_person_limit budgets person =
  List.assoc_opt (String.trim person) budgets.people

(** [get_category_amount budgets category] returns the budget for [category]
    according to [budgets], if one exists. Returns None if there is no budget
    for [category]. *)
let get_category_amount budgets category =
  Option.map (fun limit -> limit.amount) (get_category_limit budgets category)

(** [get_person_amount budgets person] returns the budget for [person] according
    to [budgets], if one exists. Returns None if there is no budget for
    [person]. *)
let get_person_amount budgets person =
  Option.map (fun limit -> limit.amount) (get_person_limit budgets person)

(** [set_category budgets category amount mode] changes the settings of
    [category] to [{amount; mode}] in [budgets]. Returns [Error] if [amount] is
    negative. *)
let set_category budgets category amount mode =
  if Money.compare amount Money.zero < 0 then
    Error "Budget must be greater than or equal to zero"
  else
    let categories =
      List.map
        (fun (current_category, current_limit) ->
          if current_category = category then (category, Some { amount; mode })
          else (current_category, current_limit))
        budgets.categories
    in
    Ok { budgets with categories }

(** [set_person budgets person amount mode] changes the settings of [person] to
    [{amount; mode}] in [budgets]. Returns [Error] if [amount] is negative. *)
let set_person budgets person amount mode =
  let person = String.trim person in
  if person = "" then Error "A person budget needs a nonempty name"
  else if Money.compare amount Money.zero < 0 then
    Error "Budget must be greater than or equal to zero"
  else
    let people =
      if List.mem_assoc person budgets.people then
        List.map
          (fun (current_person, current_limit) ->
            if current_person = person then (person, { amount; mode })
            else (current_person, current_limit))
          budgets.people
      else budgets.people @ [ (person, { amount; mode }) ]
    in
    Ok { budgets with people }

(** [category_limits budgets] returns configured category limits in
    [Types.all_categories] order. *)
let category_limits budgets =
  List.filter_map
    (fun category ->
      Option.map
        (fun limit -> (category, limit))
        (get_category_limit budgets category))
    Types.all_categories

(** [person_limits budgets] returns configured person limits in the order their
    budgets were first set. *)
let person_limits budgets = budgets.people

(** [compare_amount limit spending] compares nonnegative [spending] with
    [limit]. Returns [Overbudget] if spending exceeds the budget amount set by
    [limit]. If [limit.mode] is [Hard], returns [Within_budget] with no warnings
    when budget is close to being approached. If [limit.mode] is [Soft], the
    behavior is the following: Returns [Urgent_warning] if at most 5% of the
    budget amount set by [limit] remains, [Preliminary_warning] if at most 15%
    and above 5% of the budget amount set by [limit] remains, and
    [Within_budget] otherwise. Requires [spending >= Money.zero]. *)
let compare_amount limit spending =
  if Money.compare spending limit.amount > 0 then 
    Overbudget (Money.sub spending limit.amount)
  else
    let remaining = Money.sub limit.amount spending in
    match limit.mode with
    | Hard -> Within_budget remaining
    | Soft ->
        if Money.equal Money.zero limit.amount then Within_budget Money.zero
        else
          let percent_of_budget_left percent =
            Money.of_cents (Money.to_cents limit.amount * percent / 100)
          in
          if Money.compare remaining (percent_of_budget_left 5) <= 0 then
            Urgent_warning remaining
          else if Money.compare remaining (percent_of_budget_left 15) <= 0 then
            Preliminary_warning remaining
          else Within_budget remaining

(** [compare_category_spending budgets category spending] returns the budget
    status of [spending] in [category] if a limit exists for it, returns
    [Unbudgeted] if no such limit exists for [category]. *)
let compare_category_spending budgets category spending =
  match get_category_limit budgets category with
  | None -> Unbudgeted
  | Some limit -> compare_amount limit spending

(** [compare_person_spending budgets person spending] returns the budget status
    of [spending] for [person] if a limit exists for it, returns [Unbudgeted] if
    no such limit exists for [person]. *)
let compare_person_spending budgets person spending =
  match get_person_limit budgets person with
  | None -> Unbudgeted
  | Some limit -> compare_amount limit spending

(** [spending_in_category expenses category] is the sum of all expenses in
    [category]. *)
let spending_in_category expenses category =
  expenses
  |> List.map (fun expense ->
      if Expense.category expense = category then Expense.total expense
      else Money.zero)
  |> Money.sum

(** [spending_by_person expenses person] is the sum of the shares assigned to
    [person] across [expenses]. *)
let spending_by_person expenses person =
  expenses
  |> List.map (fun expense -> Expense.share_of expense person)
  |> Money.sum

(** [validate_hard_limits budgets expenses] returns [Ok ()] if [expenses] fit
    every hard category and person limit. Otherwise, returns an error describing
    the first violation caught. Soft limits never cause an error. Affects
    whether a request for an expense is accepted or rejected. *)
let validate_hard_limits budgets expenses =
  let category_error =
    category_limits budgets
    |> List.find_map (fun (category, limit) ->
        if limit.mode = Soft then None
        else
          let category_total = spending_in_category expenses category in
          if Money.compare category_total limit.amount <= 0 then None
          else
            Some
              (Printf.sprintf
                 "[WARNING] Prevention from Exceeding Budget: Hard %s budget \
                  set at $%s, but category spending would be $%s"
                 (Types.category_to_string category)
                 (Money.to_string limit.amount)
                 (Money.to_string category_total)))
  in
  match category_error with
  | Some message -> Error message
  | None -> (
      let person_error =
        person_limits budgets
        |> List.find_map (fun (person, limit) ->
            if limit.mode = Soft then None
            else
              let person_total = spending_by_person expenses person in
              if Money.compare person_total limit.amount <= 0 then None
              else
                Some
                  (Printf.sprintf
                     "[WARNING] Prevention from Exceeding Budget: Hard budget \
                      for %s set at $%s, but their assigned shares would be \
                      $%s"
                     person
                     (Money.to_string limit.amount)
                     (Money.to_string person_total)))
      in
      match person_error with
      | Some message -> Error message
      | None -> Ok ())
