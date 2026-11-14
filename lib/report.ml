(** Produces full user-friendly readable final report of the expense exchange
    history and results within group. *)

(** [totals_by_category expenses] returns the total monetary value of the
    expenses associated with each spending category. *)
let totals_by_category expenses =
  List.map
    (fun category ->
      let category_total =
        List.fold_left
          (fun acc expense ->
            if Expense.category expense = category then
              Money.add acc (Expense.total expense)
            else acc)
          Money.zero expenses
      in
      (category, category_total))
    Types.all_categories

(** [totals_by_person people expenses] returns the share of expenses assigned to
    each person in the group. *)
let totals_by_person people expenses =
  List.map
    (fun person ->
      let person_total =
        List.fold_left
          (fun acc expense -> Money.add acc (Expense.share_of expense person))
          Money.zero expenses
      in
      (person, person_total))
    people

(** [format_report people expenses budget] returns a human-readable summary
    report of per-category spending and budgeting statuses, as well as
    per-person assigned shares of expenses. *)
let format_report people expenses budgets =
  let category_report =
    List.map
      (fun (category, category_total) ->
        let category_name = Types.category_to_string category in
        match Budget.get budgets category with
        | None ->
            Printf.sprintf "| %s: spending ~ $%s [no budget set for category]"
              category_name
              (Money.to_string category_total)
        | Some budget ->
            let status =
              Budget.compare_spending budgets category category_total
            in
            Printf.sprintf "| %s: spending ~ $%s [%s]" category_name
              (Money.to_string category_total)
              begin match status with
              | Budget.Overbudget budget_exceeded_by ->
                  Printf.sprintf "OVERBUDGET by $%s"
                    (Money.to_string budget_exceeded_by)
              | Budget.Urgent_warning remaining ->
                  Printf.sprintf
                    "CUTTING IT CLOSE - $%s until budget reached (5%% of \
                     budget remaining)"
                    (Money.to_string remaining)
              | Budget.Preliminary_warning remaining ->
                  Printf.sprintf
                    "FAST APPROACHING - $%s until budget reached (15%% of \
                     budget remaining)"
                    (Money.to_string remaining)
              | Budget.Within_budget remaining ->
                  Printf.sprintf "WITHIN BUDGET - $%s until budget reached"
                    (Money.to_string remaining)
              | Budget.Unbudgeted -> "UNBUDGETED"
              end)
      (totals_by_category expenses)
  in
  let person_report =
    List.map
      (fun (person, person_total) ->
        Printf.sprintf " %s: $%s" person (Money.to_string person_total))
      (totals_by_person people expenses)
  in
  String.concat "\n"
    ([ "EXPENSE REPORT"; "Spending by category: " ]
    @ category_report
    @ [ ""; "Spending by person: " ]
    @ person_report)
