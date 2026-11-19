open OUnit2
open Trip_tab

let parse input =
  match Command.parse input with
  | Ok command -> command
  | Error msg -> assert_failure ("Unexpected parse error: " ^ msg)

let test_add_traveler _ =
  match parse "add-traveler Amy" with
  | Command.Add_traveler "Amy" -> ()
  | _ -> assert_failure "Expected Add_traveler"

let test_remove_traveler _ =
  match parse "remove-traveler Amy" with
  | Command.Remove_traveler "Amy" -> ()
  | _ -> assert_failure "Expected Remove_traveler"

let test_add_expense_equal_all _ =
  match parse "add-expense Amy dining 60.00 equal all | Dinner" with
  | Command.Add_expense
      ("Amy", Types.Dining, amount, Command.Equal_all, "Dinner") ->
      assert_equal 6000 (Money.to_cents amount)
  | _ -> assert_failure "Expected Add_expense with Equal_all"

let test_add_expense_exact _ =
  match
    parse "add-expense Amy dining 60.00 exact Amy=20.00 Sandy=40.00 | Dinner"
  with
  | Command.Add_expense
      ("Amy", Types.Dining, amount, Command.Exact shares, "Dinner") ->
      assert_equal 6000 (Money.to_cents amount);
      assert_equal
        [ ("Amy", 2000); ("Sandy", 4000) ]
        (List.map
           (fun (person, money) -> (person, Money.to_cents money))
           shares)
  | _ -> assert_failure "Expected Add_expense with Exact split"

let test_add_expense_equal_people _ =
  match
    parse "add-expense Amy dining 40.00 equal Sandy Jennifer | Group dinner"
  with
  | Command.Add_expense
      ("Amy", Types.Dining, amount, Command.Equal people, "Group dinner") ->
      assert_equal 4000 (Money.to_cents amount);
      assert_equal [ "Sandy"; "Jennifer" ] people
  | _ -> assert_failure "Expected equal split with description"

let test_add_bill _ =
  match
    parse
      "add-bill Amy dining tax=5.00 tip=9.00 | Dinner at Pokebowl | Chicken \
       Pasta 18.00 Amy | Garlic Bread 8.00 Amy, Sandy"
  with
  | Command.AddBill
      { payer; category; bill; description } ->
      assert_equal "Amy" payer;
      assert_equal Types.Dining category;
      assert_equal "Dinner at Pokebowl" description;
      assert_equal 500 (Money.to_cents bill.tax);
      assert_equal 900 (Money.to_cents bill.tip);
      assert_equal 2 (List.length bill.items);
      (match bill.items with
      | [ first; second ] ->
          assert_equal "Chicken Pasta" first.name;
          assert_equal 1800 (Money.to_cents first.price);
          assert_equal [ "Amy" ] first.participants;

          assert_equal "Garlic Bread" second.name;
          assert_equal 800 (Money.to_cents second.price);
          assert_equal [ "Amy"; "Sandy" ] second.participants
      | _ -> assert_failure "Expected two bill items")
  | _ -> assert_failure "Expected Add_bill"

let test_delete_expense _ =
  match parse "delete-expense 3" with
  | Command.Delete_expense 3 -> ()
  | _ -> assert_failure "Expected Delete_expense"

let test_edit_expense _ =
  match parse "edit-expense 2" with
  | Command.Edit_expense 2 -> ()
  | _ -> assert_failure "Expected Edit_expense"

let test_budget _ =
  match parse "budget dining 70.00" with
  | Command.Set_budget (Types.Dining, amount) ->
      assert_equal 7000 (Money.to_cents amount)
  | _ -> assert_failure "Expected Set_budget"

let test_balances _ =
  match parse "balances" with
  | Command.Balances -> ()
  | _ -> assert_failure "Expected Balances"

let test_help _ =
  match parse "help" with
  | Command.Help -> ()
  | _ -> assert_failure "Expected Help"

let test_quit _ =
  match parse "quit" with
  | Command.Quit -> ()
  | _ -> assert_failure "Expected Quit"

let test_invalid_command _ =
  match Command.parse "not-a-command" with
  | Error _ -> ()
  | Ok _ -> assert_failure "Expected an error for invalid command"

let test_invalid_money _ =
  match Command.parse "add-expense Amy dining abc equal all | Dinner" with
  | Error _ -> ()
  | Ok _ -> assert_failure "Expected an error for invalid amount"

let test_invalid_id _ =
  match Command.parse "delete-expense abc" with
  | Error _ -> ()
  | Ok _ -> assert_failure "Expected an error for invalid ID"

let test_missing_separator _ =
  match
    Command.parse
      "add-expense Amy dining 40.00 equal Sandy Jennifer Group dinner"
  with
  | Error _ -> ()
  | Ok _ -> assert_failure "Expected an error when separator is missing"

let test_missing_description _ =
  match Command.parse "add-expense Amy dining 40.00 equal all |" with
  | Error _ -> ()
  | Ok _ -> assert_failure "Expected an error for missing description"

let test_add_expense_exact_with_description _ =
  match
    parse
      "add-expense Amy dining 60.00 exact Amy=20 Sandy=15 Jennifer=25 | Dinner"
  with
  | Command.Add_expense
      ("Amy", Types.Dining, amount, Command.Exact shares, description) ->
      assert_equal 6000 (Money.to_cents amount);
      assert_equal
        [ ("Amy", 2000); ("Sandy", 1500); ("Jennifer", 2500) ]
        (List.map
           (fun (person, money) -> (person, Money.to_cents money))
           shares);
      assert_equal "Dinner" description
  | _ -> assert_failure "Expected exact split with Dinner as the description"

let suite =
  "Command tests"
  >::: [
         "add traveler" >:: test_add_traveler;
         "remove traveler" >:: test_remove_traveler;
         "equal all expense" >:: test_add_expense_equal_all;
         "exact expense" >:: test_add_expense_exact;
         "equal people expense" >:: test_add_expense_equal_people;
         "add bill" >:: test_add_bill;
         "delete expense" >:: test_delete_expense;
         "edit expense" >:: test_edit_expense;
         "budget" >:: test_budget;
         "balances" >:: test_balances;
         "help" >:: test_help;
         "quit" >:: test_quit;
         "invalid command" >:: test_invalid_command;
         "invalid money" >:: test_invalid_money;
         "invalid ID" >:: test_invalid_id;
         "missing separator" >:: test_missing_separator;
         "missing description" >:: test_missing_description;
         "add_expense_exact_with_description"
         >:: test_add_expense_exact_with_description;
       ]

let () = run_test_tt_main suite
