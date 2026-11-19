open OUnit2
open Trip_tab

(* helpers *)

(** [balances entries] is the balances with each [(person, cents)] in [entries],
    in order. Fails the test if [entries] is not a valid balance. *)
let balances entries =
  match
    Balance.of_list (List.map (fun (p, c) -> (p, Money.of_cents c)) entries)
  with
  | Ok b -> b
  | Error msg -> failwith ("bad test balances: " ^ msg)

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

(** [triples payments] is [payments] as (payer, payee, cents), easy to compare
    and print. *)
let triples payments =
  List.map
    (fun (p : Settle.payment) -> (p.payer, p.payee, Money.to_cents p.amount))
    payments

let print_triples ts =
  ts
  |> List.map (fun (a, b, c) -> Printf.sprintf "%s->%s %d" a b c)
  |> String.concat "; " |> Printf.sprintf "[%s]"

let cents b = List.map (fun (p, m) -> (p, Money.to_cents m)) (Balance.to_list b)

let print_cents entries =
  entries
  |> List.map (fun (p, c) -> Printf.sprintf "%s: %d" p c)
  |> String.concat "; " |> Printf.sprintf "[%s]"

(** [assert_valid_plan b payments] checks that every payment is positive and
    between two different people, and that making [payments] settles [b]. *)
let assert_valid_plan b payments =
  List.iter
    (fun (p : Settle.payment) ->
      assert_bool "amount must be positive"
        (Money.compare p.amount Money.zero > 0);
      assert_bool "payer must differ from payee" (p.payer <> p.payee))
    payments;
  assert_bool "payments must settle everyone"
    (Settle.is_settled (Settle.apply b payments))

(** [greedy_test name entries expected] checks that [Settle.greedy] on the
    balances [entries] gives exactly [expected], and that it is a valid plan. *)
let greedy_test name entries expected =
  name >:: fun _ ->
  let b = balances entries in
  let payments = Settle.greedy b in
  assert_equal expected (triples payments) ~printer:print_triples;
  assert_valid_plan b payments

(** [assert_invalid f] checks that [f ()] raises [Invalid_argument]. *)
let assert_invalid f =
  match f () with
  | _ -> assert_failure "expected Invalid_argument"
  | exception Invalid_argument _ -> ()

(* Settle.greedy *)

let greedy_tests =
  [
    greedy_test "no people" [] [];
    greedy_test "everyone already settled"
      [ ("Amy", 0); ("Sandy", 0); ("Kaylee", 0) ]
      [];
    greedy_test "one person owed by everyone, tie goes to earlier person"
      [ ("Amy", 2000); ("Sandy", -1000); ("Kaylee", -1000) ]
      [ ("Sandy", "Amy", 1000); ("Kaylee", "Amy", 1000) ];
    greedy_test "one person owes everyone, largest creditor first"
      [ ("Amy", -3000); ("Sandy", 1000); ("Kaylee", 2000) ]
      [ ("Amy", "Kaylee", 2000); ("Amy", "Sandy", 1000) ];
    greedy_test "two separate pairs"
      [ ("Amy", 500); ("Sandy", -500); ("Kaylee", 300); ("Jen", -300) ]
      [ ("Sandy", "Amy", 500); ("Jen", "Kaylee", 300) ];
    greedy_test "settled person is never mentioned"
      [ ("Amy", 100); ("Sandy", 0); ("Kaylee", -100) ]
      [ ("Kaylee", "Amy", 100) ];
    (* Worked by hand: C pays A 5 (A has 1 left); D pays B 4 (B has 1 left); A
       and B tie at 1, so E pays A first, then B. The best plan uses 3 payments
       (C->B, D->A, E->A); Step 4 finds that one. *)
    greedy_test "greedy is not always fewest"
      [ ("A", 600); ("B", 500); ("C", -500); ("D", -400); ("E", -200) ]
      [ ("C", "A", 500); ("D", "B", 400); ("E", "A", 100); ("E", "B", 100) ];
    ( "$10.00 split three ways settles exactly" >:: fun _ ->
      let b =
        Balance.compute
          [ "Amy"; "Sandy"; "Kaylee" ]
          [
            expense ~payer:"Sandy" ~total:1000
              [ ("Amy", 334); ("Sandy", 333); ("Kaylee", 333) ];
          ]
      in
      let payments = Settle.greedy b in
      assert_equal
        [ ("Amy", "Sandy", 334); ("Kaylee", "Sandy", 333) ]
        (triples payments) ~printer:print_triples;
      assert_valid_plan b payments );
  ]

(* Settle.payments *)

(** [payments_test name entries expected] checks that [Settle.payments] on the
    balances [entries] gives exactly [expected], and that it is a valid plan. *)
let payments_test name entries expected =
  name >:: fun _ ->
  let b = balances entries in
  let payments = Settle.payments b in
  assert_equal expected (triples payments) ~printer:print_triples;
  assert_valid_plan b payments

(** [a_to_e] is a situation where greedy needs 4 payments but 3 are enough:
    {A, D, E} and {B, C} each sum to zero. *)
let a_to_e = [ ("A", 600); ("B", 500); ("C", -500); ("D", -400); ("E", -200) ]

(** [pairs k] is [2k] more people in [k] pairs who each owe each other $1.00,
    named so they cannot clash with [a_to_e]. *)
let pairs k =
  List.concat
    (List.init k (fun i ->
         [ ("Owed" ^ string_of_int i, 100); ("Owes" ^ string_of_int i, -100) ]))

let payments_tests =
  [
    payments_test "no people" [] [];
    payments_test "everyone already settled"
      [ ("Amy", 0); ("Sandy", 0); ("Kaylee", 0) ]
      [];
    (* Groups {A, D, E} and {B, C}; greedy settles each group. *)
    payments_test "fewer payments than greedy" a_to_e
      [ ("D", "A", 400); ("E", "A", 200); ("C", "B", 500) ];
    (* Amy and Jen cancel exactly, and so do Sandy and Kaylee. *)
    payments_test "pairs that cancel are settled directly"
      [ ("Amy", 300); ("Sandy", 200); ("Kaylee", -200); ("Jen", -300) ]
      [ ("Jen", "Amy", 300); ("Kaylee", "Sandy", 200) ];
    payments_test "one group of three, no pair cancels"
      [ ("Amy", 500); ("Sandy", -200); ("Kaylee", -300) ]
      [ ("Kaylee", "Amy", 300); ("Sandy", "Amy", 200) ];
    payments_test "one person owed by everyone"
      [ ("Amy", 2000); ("Sandy", -1000); ("Kaylee", -1000) ]
      [ ("Sandy", "Amy", 1000); ("Kaylee", "Amy", 1000) ];
    payments_test "settled people are skipped"
      [ ("Amy", 100); ("Sandy", 0); ("Kaylee", -100) ]
      [ ("Kaylee", "Amy", 100) ];
    ( "12 people are still searched" >:: fun _ ->
      let entries =
        a_to_e @ pairs 2 @ [ ("J", 300); ("K", -200); ("L", -100) ]
      in
      (* Groups: {A, D, E}, {B, C}, two pairs, {J, K, L}: 5 groups, so 12 - 5 =
         7 payments. *)
      let b = balances entries in
      let payments = Settle.payments b in
      assert_equal 7 (List.length payments) ~printer:string_of_int;
      assert_valid_plan b payments );
    ( "more than 12 people falls back to greedy" >:: fun _ ->
      let b = balances (a_to_e @ pairs 4) in
      assert_equal
        (triples (Settle.greedy b))
        (triples (Settle.payments b))
        ~printer:print_triples );
  ]

(* apply, is_settled, to_string *)

let pay payer payee cents : Settle.payment =
  { payer; payee; amount = Money.of_cents cents }

let misc_tests =
  [
    ( "apply a partial payment" >:: fun _ ->
      let b = balances [ ("Amy", 500); ("Sandy", -500) ] in
      assert_equal
        [ ("Amy", 300); ("Sandy", -300) ]
        (cents (Settle.apply b [ pay "Sandy" "Amy" 200 ]))
        ~printer:print_cents );
    ( "apply no payments changes nothing" >:: fun _ ->
      let b = balances [ ("Amy", 500); ("Sandy", -500) ] in
      assert_equal
        [ ("Amy", 500); ("Sandy", -500) ]
        (cents (Settle.apply b []))
        ~printer:print_cents );
    ( "apply with unknown payer raises" >:: fun _ ->
      let b = balances [ ("Amy", 500); ("Sandy", -500) ] in
      assert_invalid (fun () -> Settle.apply b [ pay "Zed" "Amy" 100 ]) );
    ( "apply with unknown payee raises" >:: fun _ ->
      let b = balances [ ("Amy", 500); ("Sandy", -500) ] in
      assert_invalid (fun () -> Settle.apply b [ pay "Sandy" "Zed" 100 ]) );
    ( "is_settled on all zero" >:: fun _ ->
      assert_bool "should be settled"
        (Settle.is_settled (balances [ ("Amy", 0); ("Sandy", 0) ])) );
    ( "is_settled on no people" >:: fun _ ->
      assert_bool "should be settled" (Settle.is_settled (balances [])) );
    ( "is_settled when someone owes" >:: fun _ ->
      assert_bool "should not be settled"
        (not (Settle.is_settled (balances [ ("Amy", 1); ("Sandy", -1) ]))) );
    ( "to_string with payments" >:: fun _ ->
      assert_equal ~printer:Fun.id
        "Sandy pays Amy $10.00\nKaylee pays Amy $0.05"
        (Settle.to_string [ pay "Sandy" "Amy" 1000; pay "Kaylee" "Amy" 5 ]) );
    ( "to_string with no payments" >:: fun _ ->
      assert_equal ~printer:Fun.id "Everyone is settled up."
        (Settle.to_string []) );
  ]

(* randomized property tests *)

type random_expense = {
  paid_by : int;  (** index of the payer *)
  split : (int * int) list;  (** (index of person, share in cents) *)
}
(** A randomly generated expense, before it is turned into an [Expense.t].
    People are numbered [0] to [n - 1] and named by {!traveler_name}. *)

(** [traveler_name i] is the name of the [i]th random traveler, e.g. ["P3"]. *)
let traveler_name i = "P" ^ string_of_int i

(** [gaps points] is the differences between consecutive elements of [points],
    e.g. [gaps [0; 3; 3; 10]] is [[3; 0; 7]]. *)
let rec gaps = function
  | a :: (b :: _ as rest) -> (b - a) :: gaps rest
  | _ -> []

(** [amounts_gen total k] generates [k] nonnegative amounts that sum to exactly
    [total], by cutting [0..total] at [k - 1] random points. Requires: [k >= 1]
    and [total >= 0]. *)
let amounts_gen total k =
  let open QCheck.Gen in
  list_size (return (k - 1)) (int_bound total) >|= fun cuts ->
  gaps ((0 :: List.sort compare cuts) @ [ total ])

(** [expense_gen n] generates a random valid expense among [n] travelers: a
    random payer, a total between $0.01 and $1000.00, and a random nonempty set
    of participants with random (possibly zero) shares. Requires: [n >= 1]. *)
let expense_gen n =
  let open QCheck.Gen in
  int_bound (n - 1) >>= fun paid_by ->
  int_range 1 100_000 >>= fun total ->
  int_range 1 n >>= fun k ->
  shuffle_list (List.init n Fun.id) >>= fun people ->
  amounts_gen total k >|= fun amounts ->
  {
    paid_by;
    split = List.combine (List.filteri (fun i _ -> i < k) people) amounts;
  }

(** [trip_gen] generates [(n, expenses)]: between 1 and 8 travelers and up to 15
    random expenses among them. *)
let trip_gen =
  let open QCheck.Gen in
  int_range 1 8 >>= fun n ->
  list_size (int_bound 15) (expense_gen n) >|= fun expenses -> (n, expenses)

let print_random_expense e =
  Printf.sprintf "%s paid {%s}" (traveler_name e.paid_by)
    (e.split
    |> List.map (fun (p, c) -> Printf.sprintf "%s=%d" (traveler_name p) c)
    |> String.concat ", ")

let print_trip (n, expenses) =
  Printf.sprintf "%d travelers: %s" n
    (String.concat "; " (List.map print_random_expense expenses))

(** [arbitrary_trip] is {!trip_gen} with a printer, so a failing test shows the
    trip that broke it. *)
let arbitrary_trip = QCheck.make ~print:print_trip trip_gen

(** [to_expenses expenses] turns random expenses into real ones, with ids [0],
    [1], .... *)
let to_expenses expenses =
  List.mapi
    (fun id e ->
      let shares = List.map (fun (p, c) -> (traveler_name p, c)) e.split in
      expense ~id ~payer:(traveler_name e.paid_by)
        ~total:(List.fold_left (fun acc (_, c) -> acc + c) 0 e.split)
        shares)
    expenses

(** [balances_of (n, expenses)] is the balances of a random trip. *)
let balances_of (n, expenses) =
  Balance.compute (List.init n traveler_name) (to_expenses expenses)

let count_nonzero b =
  List.length
    (List.filter
       (fun (_, m) -> not (Money.equal m Money.zero))
       (Balance.to_list b))

(** [property name prop] is an OUnit test that checks [prop] on 500 random
    trips. *)
let property name prop =
  QCheck_ounit.to_ounit2_test
    (QCheck.Test.make ~count:500 ~name arbitrary_trip prop)

let property_tests =
  [
    property "balances always sum to zero" (fun trip ->
        let b = balances_of trip in
        Money.equal (Money.sum (List.map snd (Balance.to_list b))) Money.zero);
    property "expense order does not change balances" (fun (n, expenses) ->
        cents (balances_of (n, expenses))
        = cents (balances_of (n, List.rev expenses)));
    property "greedy payments settle everyone exactly" (fun trip ->
        let b = balances_of trip in
        Settle.is_settled (Settle.apply b (Settle.greedy b)));
    property "greedy payments are positive and between different people"
      (fun trip ->
        List.for_all
          (fun (p : Settle.payment) ->
            Money.compare p.amount Money.zero > 0 && p.payer <> p.payee)
          (Settle.greedy (balances_of trip)));
    property "greedy uses at most (nonzero balances - 1) payments" (fun trip ->
        let b = balances_of trip in
        List.length (Settle.greedy b) <= max 0 (count_nonzero b - 1));
    property "nobody both pays and receives" (fun trip ->
        let payments = Settle.greedy (balances_of trip) in
        List.for_all
          (fun (p : Settle.payment) ->
            List.for_all
              (fun (q : Settle.payment) -> q.payee <> p.payer)
              payments)
          payments);
    property "payments settle everyone exactly" (fun trip ->
        let b = balances_of trip in
        Settle.is_settled (Settle.apply b (Settle.payments b)));
    property "payments never uses more payments than greedy" (fun trip ->
        let b = balances_of trip in
        List.length (Settle.payments b) <= List.length (Settle.greedy b));
    property "payments are positive and nobody both pays and receives"
      (fun trip ->
        let payments = Settle.payments (balances_of trip) in
        List.for_all
          (fun (p : Settle.payment) ->
            Money.compare p.amount Money.zero > 0
            && p.payer <> p.payee
            && List.for_all
                 (fun (q : Settle.payment) -> q.payee <> p.payer)
                 payments)
          payments);
  ]

let tests =
  "settle tests"
  >::: greedy_tests @ payments_tests @ misc_tests @ property_tests

let _ = run_test_tt_main tests
