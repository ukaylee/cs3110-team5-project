open OUnit2
open Trip_tab

let test_single_person _ =
  let bill : Split.bill =
    {
      items =
        [
          {
            name = "Pizza";
            price = Money.of_cents 1000;
            participants = [ "Amy" ];
          };
        ];
      tax = Money.zero;
      tip = Money.zero;
    }
  in
  let result = Split.itemized 1 "Dinner" "Amy" Types.Dining bill in
  match result with
  | Error msg -> assert_failure msg
  | Ok expense -> assert_equal 1000 (Money.to_cents (Expense.total expense))

let test_even_split _ =
  let bill : Split.bill =
    {
      items =
        [
          {
            name = "Pizza";
            price = Money.of_cents 900;
            participants = [ "Amy"; "Sandy"; "Jennifer" ];
          };
        ];
      tax = Money.zero;
      tip = Money.zero;
    }
  in
  let result = Split.itemized 1 "Dinner" "Amy" Types.Dining bill in
  match result with
  | Error msg -> assert_failure msg
  | Ok expense ->
      assert_equal 900 (Money.to_cents (Expense.total expense));

      let shares = Expense.shares expense in

      assert_equal 300 (Money.to_cents (List.assoc "Amy" shares));

      assert_equal 300 (Money.to_cents (List.assoc "Sandy" shares));

      assert_equal 300 (Money.to_cents (List.assoc "Jennifer" shares))

let test_multiple_items _ =
  let bill : Split.bill =
    {
      items =
        [
          {
            name = "Cookie";
            price = Money.of_cents 100;
            participants = [ "Cookie Monster" ];
          };
          {
            name = "Donut";
            price = Money.of_cents 300;
            participants = [ "Donut Monster"; "Cookie Monster" ];
          };
        ];
      tax = Money.zero;
      tip = Money.zero;
    }
  in
  let result = Split.itemized 1 "Dinner" "Cookie Monster" Types.Dining bill in
  match result with
  | Error msg -> assert_failure msg
  | Ok expense ->
      assert_equal 400 (Money.to_cents (Expense.total expense));

      let shares = Expense.shares expense in

      assert_equal 250 (Money.to_cents (List.assoc "Cookie Monster" shares));

      assert_equal 150 (Money.to_cents (List.assoc "Donut Monster" shares))

let test_tax_and_tip _ =
  let bill : Split.bill =
    {
      items =
        [
          {
            name = "Pizza";
            price = Money.of_cents 2000;
            participants = [ "Amy" ];
          };
          {
            name = "Pasta";
            price = Money.of_cents 1000;
            participants = [ "Sandy" ];
          };
        ];
      tax = Money.of_cents 300;
      tip = Money.of_cents 300;
    }
  in
  let result = Split.itemized 1 "Dinner" "Amy" Types.Dining bill in
  match result with
  | Error msg -> assert_failure msg
  | Ok expense ->
      assert_equal 3600 (Money.to_cents (Expense.total expense));

      let shares = Expense.shares expense in
      assert_equal 2400 (Money.to_cents (List.assoc "Amy" shares));
      assert_equal 1200 (Money.to_cents (List.assoc "Sandy" shares))

let test_different_payer _ =
  let bill : Split.bill =
    {
      items =
        [
          {
            name = "Pizza";
            price = Money.of_cents 1000;
            participants = [ "Amy" ];
          };
          {
            name = "Pasta";
            price = Money.of_cents 2000;
            participants = [ "Sandy" ];
          };
        ];
      tax = Money.zero;
      tip = Money.zero;
    }
  in
  let result = Split.itemized 1 "Dinner" "Jennifer" Types.Dining bill in
  match result with
  | Error msg -> assert_failure msg
  | Ok expense ->
      assert_equal "Jennifer" (Expense.payer expense);
      assert_equal 3000 (Money.to_cents (Expense.total expense));

      let shares = Expense.shares expense in
      assert_equal 1000 (Money.to_cents (List.assoc "Amy" shares));
      assert_equal 2000 (Money.to_cents (List.assoc "Sandy" shares))

let test_tax_tip_rounding _ =
  let bill : Split.bill =
    {
      items =
        [
          {
            name = "Cookie";
            price = Money.of_cents 300;
            participants = [ "Amy"; "Sandy"; "Jennifer" ];
          };
        ];
      tax = Money.of_cents 1;
      tip = Money.of_cents 1;
    }
  in
  let result = Split.itemized 1 "Dinner" "Amy" Types.Dining bill in
  match result with
  | Error msg -> assert_failure msg
  | Ok expense ->
      assert_equal 302 (Money.to_cents (Expense.total expense));

      let shares = Expense.shares expense in
      assert_equal 3 (List.length shares);

      List.iter
        (fun person ->
          assert_bool
            ("Missing participant: " ^ person)
            (List.mem_assoc person shares))
        [ "Amy"; "Sandy"; "Jennifer" ];

      assert_equal 302 (Money.to_cents (Money.sum (List.map snd shares)))

let assert_invalid_bill bill =
  match Split.itemized 1 "Dinner" "Amy" Types.Dining bill with
  | Error _ -> ()
  | Ok _ -> assert_failure "Expected an invalid bill to return Error"

let test_empty_bill _ =
  let bill : Split.bill = { items = []; tax = Money.zero; tip = Money.zero } in
  assert_invalid_bill bill

let test_empty_participants _ =
  let bill : Split.bill =
    {
      items =
        [ { name = "Pizza"; price = Money.of_cents 1000; participants = [] } ];
      tax = Money.zero;
      tip = Money.zero;
    }
  in
  assert_invalid_bill bill

let test_negative_price _ =
  let bill : Split.bill =
    {
      items =
        [
          {
            name = "Pizza";
            price = Money.of_cents (-500);
            participants = [ "Amy" ];
          };
        ];
      tax = Money.zero;
      tip = Money.zero;
    }
  in
  assert_invalid_bill bill

let test_negative_tax _ =
  let bill : Split.bill =
    {
      items =
        [
          {
            name = "Pizza";
            price = Money.of_cents 1000;
            participants = [ "Amy" ];
          };
        ];
      tax = Money.of_cents (-100);
      tip = Money.zero;
    }
  in
  assert_invalid_bill bill

let test_negative_tip _ =
  let bill : Split.bill =
    {
      items =
        [
          {
            name = "Pizza";
            price = Money.of_cents 1000;
            participants = [ "Amy" ];
          };
        ];
      tax = Money.zero;
      tip = Money.of_cents (-100);
    }
  in
  assert_invalid_bill bill

let test_duplicate_participants _ =
  let bill : Split.bill =
    {
      items =
        [
          {
            name = "Pizza";
            price = Money.of_cents 1000;
            participants = [ "Amy"; "Amy" ];
          };
        ];
      tax = Money.zero;
      tip = Money.zero;
    }
  in
  assert_invalid_bill bill

let test_zero_subtotal _ =
  let bill : Split.bill =
    {
      items =
        [ { name = "Pizza"; price = Money.zero; participants = [ "Amy" ] } ];
      tax = Money.of_cents 100;
      tip = Money.zero;
    }
  in
  assert_invalid_bill bill

let suite =
  "Split tests"
  >::: [
         "single person" >:: test_single_person;
         "even split" >:: test_even_split;
         "multiple items" >:: test_multiple_items;
         "tax and tip" >:: test_tax_and_tip;
         "different payer" >:: test_different_payer;
         "tax tip rounding" >:: test_tax_tip_rounding;
         "empty bill" >:: test_empty_bill;
         "empty participants" >:: test_empty_participants;
         "negative price" >:: test_negative_price;
         "negative tax" >:: test_negative_tax;
         "negative tip" >:: test_negative_tip;
         "duplicate participants" >:: test_duplicate_participants;
         "zero subtotal" >:: test_zero_subtotal;
       ]

let () = run_test_tt_main suite
