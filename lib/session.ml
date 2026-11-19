type t = { trip : Trip.t }

let create trip = { trip }
let trip session = session.trip

(* ACTIONS *)
let add_expense session expense =
  match Trip.add_expense session.trip expense with
  | Error msg -> Error msg
  | Ok updated_trip -> Ok { trip = updated_trip }

let add_traveler session person =
  match Trip.add_traveler session.trip person with
  | Error msg -> Error msg
  | Ok updated_trip -> Ok { trip = updated_trip }

let remove_traveler session person =
  match Trip.remove_traveler session.trip person with
  | Error msg -> Error msg
  | Ok updated_trip -> Ok { trip = updated_trip }

let delete_expense session id =
  match Trip.delete_expense session.trip id with
  | Error msg -> Error msg
  | Ok updated_trip -> Ok { trip = updated_trip }

let edit_expense session expense =
  match Trip.edit_expense session.trip expense with
  | Error msg -> Error msg
  | Ok updated_trip -> Ok { trip = updated_trip }

let set_budget session category amount =
  match Trip.set_budget session.trip category amount with
  | Error msg -> Error msg
  | Ok updated_trip -> Ok { trip = updated_trip }

let add_bill session description payer category bill =
  let id = Trip.next_id session.trip in
  match Split.itemized id description payer category bill with
  | Error msg -> Error msg
  | Ok expense -> add_expense session expense

(* HELPERS *)

let calculate_shares session amount split_spec =
  let people =
    match split_spec with
    (*split spec is the way people wanted to split it*)
    | Command.Equal_all -> Trip.travelers session.trip
    | Command.Equal people -> people
    | Command.Exact _ -> []
  in
  match split_spec with
  | Command.Exact shares -> Ok shares
  | Command.Equal_all | Command.Equal _ ->
      if people = [] then Error "Cannot split an expense among zero travelers."
      else if Money.compare amount Money.zero < 0 then
        Error "Cannot split a negative expense."
      else
        let amounts = Money.split_evenly amount (List.length people) in
        Ok (List.combine people amounts)

(** [convert_bill bill] converts a Command.bill into a Split.bill. *)
let convert_bill (bill : Command.bill) : Split.bill =
  {
    items =
      List.map
        (fun (item : Command.bill_item) ->
          {
            Split.name = item.name;
            price = item.price;
            participants = item.participants;
          })
        bill.items;
    tax = bill.tax;
    tip = bill.tip;
  }

let action_with_message result message =
  match result with
  | Ok updated_session -> Ok (updated_session, message)
  | Error msg -> Error msg

(** [apply session command] executes [command] on [session]. Returns the updated
    session and a success message, or an error message if the command cannot be
    completed. *)
let apply session command =
  match command with
  | Command.Add_traveler person ->
      action_with_message
        (add_traveler session person)
        ("Added traveler: " ^ person)
  | Command.Remove_traveler person ->
      action_with_message
        (remove_traveler session person)
        ("Removed traveler: " ^ person)
  | Command.Add_expense (payer, category, amount, split_spec, description) -> (
      let id = Trip.next_id session.trip in
      match calculate_shares session amount split_spec with
      | Error msg -> Error msg
      | Ok shares -> (
          match
            Expense.make ~id ~description ~payer ~category ~total:amount ~shares
          with
          | Error msg -> Error msg
          | Ok expense ->
              action_with_message
                (add_expense session expense)
                ("Added expense: " ^ description)))
  | Command.AddBill { description; payer; category; bill } ->
      let split_bill = convert_bill bill in
      action_with_message
        (add_bill session description payer category split_bill)
        ("Added bill: " ^ description)
  | Command.Edit_expense _ -> Error "Command not yet implemented"
  | Command.Delete_expense id ->
      action_with_message
        (delete_expense session id)
        ("Deleted expense: " ^ string_of_int id)
  | Command.List_expenses -> Error "Command not yet implemented"
  | Command.Balances -> Error "Command not yet implemented"
  | Command.Settle -> Error "Command not yet implemented"
  | Command.Set_budget (category, amount) ->
      action_with_message (set_budget session category amount) "Budget updated"
  | Command.Report -> Error "Command not yet implemented"
  | Command.Help -> Error "Command not yet implemented"
  | Command.Quit -> Ok (session, "Goodbye!")
