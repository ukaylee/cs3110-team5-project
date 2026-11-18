type t = { trip : Trip.t }

let create trip = { trip }
let trip session = session.trip

let add_bill session description payer category bill =
  let id = Trip.next_id session.trip in
  match Split.itemized id description payer category bill with
  | Error msg -> Error msg
  | Ok expense -> (
      match Trip.add_expense session.trip expense with
      | Error msg -> Error msg
      | Ok updated_trip -> Ok { trip = updated_trip })
