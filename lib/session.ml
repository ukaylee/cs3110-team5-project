type t = { trip : Trip.t }

let create trip = { trip }
let trip session = session.trip

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

(** [apply session command] executes [command] on [session]. Returns the updated
    session and a success message, or an error message if the command cannot be
    completed. *)
let apply session command =
  match command with
  | Command.AddBill { description; payer; category; bill } -> (
      match add_bill session description payer category bill with
      | Ok updated_session -> Ok (updated_session, "Added bill: " ^ description)
      | Error msg -> Error msg)
  | Command.Quit -> Ok (session, "Goodbye!")
