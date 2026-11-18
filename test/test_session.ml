open OUnit2
open Trip_tab

let test_add_bill _ =
  let trip = Trip.create ~name:"Vacation" in

  let trip =
    match Trip.add_traveler trip "Amy" with
    | Ok trip -> trip
    | Error msg -> assert_failure msg
  in

  let session = Session.create trip in

  let bill : Split.bill =
    {
      items =
        [
          {
            name = "Pasta";
            price = Money.of_cents 1800;
            participants = [ "Amy" ];
          };
        ];
      tax = Money.of_cents 500;
      tip = Money.of_cents 900;
    }
  in

  match Session.add_bill session "Dinner" "Amy" Types.Dining bill with
  | Error msg -> assert_failure msg
  | Ok updated_session ->
      let updated_trip = Session.trip updated_session in
      let expenses = Trip.expenses updated_trip in
      assert_equal 1 (List.length expenses);
      assert_equal 3200 (Money.to_cents (Expense.total (List.hd expenses)))

let suite = "Session tests" >::: [ "add bill" >:: test_add_bill ]
let () = run_test_tt_main suite
