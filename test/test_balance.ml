open OUnit2
open Trip_tab

let abc = [ "Amy"; "Sandy"; "Kaylee" ]

(** [expense ~payer ~total shares] is an expense in which [payer] paid [total]
    cents and each [(person, cents)] in [shares] owes that much. Fails the test
    if the expense is invalid. *)
let expense ?(id = 1) ~payer ~total shares =
  match
    Expense.make ~id ~description:"test" ~payer ~category:Types.Other
      ~total:(Money.of_cents total)
      ~shares:(List.map (fun (p, c) -> (p, Money.of_cents c)) shares)
  with
  | Ok e -> e
  | Error msg -> failwith ("bad test expense: " ^ msg)

(** [cents b] is [b] as (person, cents) pairs, easy to compare and print. *)
let cents b = List.map (fun (p, m) -> (p, Money.to_cents m)) (Balance.to_list b)

let print_cents entries =
  entries
  |> List.map (fun (p, c) -> Printf.sprintf "%s: %d" p c)
  |> String.concat "; " |> Printf.sprintf "[%s]"

let sum_cents b = List.fold_left (fun acc (_, c) -> acc + c) 0 (cents b)

(** [compute_test name travelers expenses expected] checks that
    [Balance.compute travelers expenses] is [expected] (in cents) and that the
    balances sum to zero. *)
let compute_test name travelers expenses expected =
  name >:: fun _ ->
  let b = Balance.compute travelers expenses in
  assert_equal expected (cents b) ~printer:print_cents;
  assert_equal 0 (sum_cents b) ~printer:string_of_int

(** [assert_invalid f] checks that [f ()] raises [Invalid_argument]. *)
let assert_invalid f =
  match f () with
  | _ -> assert_failure "expected Invalid_argument"
  | exception Invalid_argument _ -> ()

let compute_tests =
  [
    compute_test "no travelers, no expenses" [] [] [];
    compute_test "no expenses: everyone is even" abc []
      [ ("Amy", 0); ("Sandy", 0); ("Kaylee", 0) ];
    compute_test "Amy pays $30 split three ways" abc
      [
        expense ~payer:"Amy" ~total:3000
          [ ("Amy", 1000); ("Sandy", 1000); ("Kaylee", 1000) ];
      ]
      [ ("Amy", 2000); ("Sandy", -1000); ("Kaylee", -1000) ];
    compute_test "payer not sharing; Kaylee in no expense" abc
      [ expense ~payer:"Amy" ~total:1500 [ ("Sandy", 1500) ] ]
      [ ("Amy", 1500); ("Sandy", -1500); ("Kaylee", 0) ];
    compute_test "paying only for yourself changes nothing" abc
      [ expense ~payer:"Sandy" ~total:999 [ ("Sandy", 999) ] ]
      [ ("Amy", 0); ("Sandy", 0); ("Kaylee", 0) ];
    compute_test "$10.00 three ways, leftover cent to Amy" abc
      [
        expense ~payer:"Sandy" ~total:1000
          [ ("Amy", 334); ("Sandy", 333); ("Kaylee", 333) ];
      ]
      [ ("Amy", -334); ("Sandy", 667); ("Kaylee", -333) ];
    compute_test "two expenses that cancel out" abc
      [
        expense ~id:1 ~payer:"Amy" ~total:2000
          [ ("Amy", 1000); ("Sandy", 1000) ];
        expense ~id:2 ~payer:"Sandy" ~total:2000
          [ ("Amy", 1000); ("Sandy", 1000) ];
      ]
      [ ("Amy", 0); ("Sandy", 0); ("Kaylee", 0) ];
    compute_test "several expenses accumulate" abc
      [
        expense ~id:1 ~payer:"Amy" ~total:3000
          [ ("Amy", 1000); ("Sandy", 1000); ("Kaylee", 1000) ];
        expense ~id:2 ~payer:"Sandy" ~total:1200 [ ("Kaylee", 1200) ];
      ]
      [ ("Amy", 2000); ("Sandy", 200); ("Kaylee", -2200) ];
    compute_test "order follows travelers, not expenses"
      [ "Kaylee"; "Amy"; "Sandy" ]
      [ expense ~payer:"Amy" ~total:1500 [ ("Sandy", 1500) ] ]
      [ ("Kaylee", 0); ("Amy", 1500); ("Sandy", -1500) ];
    ( "unknown payer raises" >:: fun _ ->
      assert_invalid (fun () ->
          Balance.compute abc
            [ expense ~payer:"Zed" ~total:100 [ ("Amy", 100) ] ]) );
    ( "unknown sharer raises" >:: fun _ ->
      assert_invalid (fun () ->
          Balance.compute abc
            [ expense ~payer:"Amy" ~total:100 [ ("Zed", 100) ] ]) );
    ( "duplicate traveler raises" >:: fun _ ->
      assert_invalid (fun () -> Balance.compute [ "Amy"; "Amy" ] []) );
  ]

let m = Money.of_cents

let misc_tests =
  [
    ( "of_list accepts balances summing to zero" >:: fun _ ->
      match Balance.of_list [ ("Amy", m 500); ("Sandy", m (-500)) ] with
      | Ok b -> assert_equal [ ("Amy", 500); ("Sandy", -500) ] (cents b)
      | Error msg -> assert_failure msg );
    ( "of_list rejects a nonzero sum" >:: fun _ ->
      assert_bool "should be Error"
        (Result.is_error (Balance.of_list [ ("Amy", m 1) ])) );
    ( "of_list rejects a duplicate person" >:: fun _ ->
      assert_bool "should be Error"
        (Result.is_error (Balance.of_list [ ("Amy", m 5); ("Amy", m (-5)) ])) );
    ( "net of a present and an absent person" >:: fun _ ->
      let b =
        Balance.compute abc
          [ expense ~payer:"Amy" ~total:1500 [ ("Sandy", 1500) ] ]
      in
      assert_equal (-1500) (Money.to_cents (Balance.net b "Sandy"));
      assert_equal 0 (Money.to_cents (Balance.net b "Zed")) );
    ( "to_string with owed, owing, and settled" >:: fun _ ->
      let b =
        Balance.compute abc
          [ expense ~payer:"Amy" ~total:1500 [ ("Sandy", 1500) ] ]
      in
      assert_equal ~printer:Fun.id
        "Amy is owed $15.00\nSandy owes $15.00\nKaylee is settled up"
        (Balance.to_string b) );
    ( "to_string with no travelers" >:: fun _ ->
      assert_equal ~printer:Fun.id "No travelers yet."
        (Balance.to_string (Balance.compute [] [])) );
  ]

let tests = "balance tests" >::: compute_tests @ misc_tests
let _ = run_test_tt_main tests
