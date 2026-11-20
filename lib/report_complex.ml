(** Produces full user-friendly readable final report of the expense exchange
    history and results within group. *)

(** [totals_by_category expenses] returns the total monetary value of the
    [expenses] associated with each spending category. *)
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

(** [totals_by_person people expenses] returns the share of [expenses] assigned
    to each person in [people]. *)
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

(** [format_report people expenses budgets] returns a human-readable summary
    report of per-category spending and budgeting statuses, as well as
    per-person assigned shares and budgeting statuses. *)
let format_report people expenses budgets =
  let status_text status =
    match status with
    | Budget_complex.Overbudget overage ->
        Printf.sprintf "OVERBUDGET by $%s" (Money.to_string overage)
    | Budget_complex.Urgent_warning remaining ->
        Printf.sprintf "URGENT - only $%s remaining" (Money.to_string remaining)
    | Budget_complex.Preliminary_warning remaining ->
        Printf.sprintf "APPROACHING LIMIT - $%s remaining"
          (Money.to_string remaining)
    | Budget_complex.Within_budget remaining ->
        Printf.sprintf "WITHIN BUDGET - $%s left" (Money.to_string remaining)
    | Budget_complex.Unbudgeted -> "NO BUDGET SET"
    (* branch never reached, only here for pattern match exhaustiveness*)
  in
  let category_report =
    List.map
      (fun (category, category_total) ->
        let category_name =
          "["
          ^ (Types.category_to_string category |> String.capitalize_ascii)
          ^ "]"
        in
        match Budget_complex.get_category_limit budgets category with
        | None ->
            Printf.sprintf
              "%s\n  Spent     $%s\n  Budget    Not set\n  Status    N/A"
              category_name
              (Money.to_string category_total)
        | Some budget ->
            let status =
              Budget_complex.compare_category_spending budgets category
                category_total
            in
            let mode =
              match budget.mode with
              | Budget_complex.Soft -> "SOFT"
              | Budget_complex.Hard -> "HARD"
            in
            Printf.sprintf
              "%s\n  Spent     $%s\n  Budget    $%s (%s)\n  Status    %s"
              category_name
              (Money.to_string category_total)
              (Money.to_string budget.amount)
              mode (status_text status))
      (totals_by_category expenses)
  in
  let person_report =
    List.map
      (fun (person, person_total) ->
        match Budget_complex.get_person_limit budgets person with
        | None ->
            Printf.sprintf
              "%s ~\n  Share     $%s\n  Budget    Not set\n  Status    N/A"
              person
              (Money.to_string person_total)
        | Some budget ->
            let status =
              Budget_complex.compare_person_spending budgets person person_total
            in
            let mode =
              match budget.mode with
              | Budget_complex.Soft -> "SOFT"
              | Budget_complex.Hard -> "HARD"
            in
            Printf.sprintf
              "%s ~\n  Share     $%s\n  Budget    $%s (%s)\n  Status    %s"
              person
              (Money.to_string person_total)
              (Money.to_string budget.amount)
              mode (status_text status))
      (totals_by_person people expenses)
  in
  String.concat "\n"
    ([
       "TRIP EXPENSE REPORT";
       "===================";
       "";
       "SPENDING BY CATEGORY";
       "-----------------";
     ]
    @ List.concat_map (fun entry -> [ entry; "" ]) category_report
    @ [ "ASSIGNED SHARES BY TRAVELER"; "---------------------------" ]
    @ List.concat_map (fun entry -> [ entry; "" ]) person_report)
