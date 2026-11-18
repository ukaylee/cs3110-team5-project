(** Turns balances into transfers with an optimized settlement order. *)

(** Settling up: turning net balances into payments. *)

type payment = {
  payer : Types.person;
  payee : Types.person;
  amount : Money.t;
}

(* greedy settle-up *)

type pending = {
  position : int;
  person : Types.person;
  remaining : Money.t;
}
(** A person who still has money to pay or to receive. [position] is their index
    in the balances (used to break ties), and [remaining] is how much they still
    have to pay or receive. Invariant: [remaining] is strictly positive. *)

(** [compare_pending a b] orders people by who should be handled first: larger
    [remaining] first, and on a tie, smaller [position] first. *)
let compare_pending a b =
  let by_amount = Money.compare b.remaining a.remaining in
  if by_amount <> 0 then by_amount else Int.compare a.position b.position

(** [split_by_sign balances] is [(debtors, creditors)]: everyone in [balances]
    with a negative balance (with [remaining] equal to how much they owe) and
    everyone with a positive balance (with [remaining] equal to how much they
    are owed). *)
let split_by_sign balances =
  let pending =
    List.mapi
      (fun position (person, balance) ->
        { position; person; remaining = balance })
      (Balance.to_list balances)
  in
  let debtors =
    pending
    |> List.filter (fun p -> Money.compare p.remaining Money.zero < 0)
    |> List.map (fun p -> { p with remaining = Money.neg p.remaining })
    |> List.sort compare_pending
  in
  let creditors =
    pending
    |> List.filter (fun p -> Money.compare p.remaining Money.zero > 0)
    |> List.sort compare_pending
  in
  (debtors, creditors)

(** [reinsert p handled rest] is [rest] with [p] put back in sorted position
    after [handled] of [p.remaining] has been paid or received, or just [rest]
    if [p] has nothing left. Requires: [rest] is sorted by {!compare_pending},
    and [handled] is at most [p.remaining]. *)
let reinsert p handled rest =
  let remaining = Money.sub p.remaining handled in
  if Money.equal remaining Money.zero then rest
  else List.merge compare_pending [ { p with remaining } ] rest

(** [min_money a b] is the smaller of [a] and [b]. *)
let min_money a b = if Money.compare a b <= 0 then a else b

(** [match_up debtors creditors] is the greedy payments that settle [debtors]
    with [creditors]. Requires: both lists are sorted by {!compare_pending}, and
    the total of [debtors] equals the total of [creditors]. *)
let rec match_up debtors creditors =
  match (debtors, creditors) with
  | debtor :: other_debtors, creditor :: other_creditors ->
      let amount = min_money debtor.remaining creditor.remaining in
      { payer = debtor.person; payee = creditor.person; amount }
      :: match_up
           (reinsert debtor amount other_debtors)
           (reinsert creditor amount other_creditors)
  | _ -> []

let greedy balances =
  let debtors, creditors = split_by_sign balances in
  match_up debtors creditors

(* applying payments *)

(** [check_person balances person] is [()] if [person] is in [balances].
    @raise Invalid_argument otherwise. *)
let check_person balances person =
  if not (List.mem_assoc person (Balance.to_list balances)) then
    invalid_arg
      (Printf.sprintf "Settle.apply: %s is not in the balances" person)

(** [change_from payments person] is how much [payments] raise [person]'s
    balance: the total they pay minus the total they receive. *)
let change_from payments person =
  List.fold_left
    (fun acc payment ->
      let acc =
        if payment.payer = person then Money.add acc payment.amount else acc
      in
      if payment.payee = person then Money.sub acc payment.amount else acc)
    Money.zero payments

let apply balances payments =
  List.iter
    (fun payment ->
      check_person balances payment.payer;
      check_person balances payment.payee)
    payments;
  let updated =
    List.map
      (fun (person, balance) ->
        (person, Money.add balance (change_from payments person)))
      (Balance.to_list balances)
  in
  (* Every payment adds and subtracts the same amount, so [updated] still sums
     to zero and has no duplicates; [of_list] cannot fail. *)
  match Balance.of_list updated with
  | Ok result -> result
  | Error msg -> failwith ("Settle.apply: impossible: " ^ msg)

let is_settled balances =
  List.for_all
    (fun (_, balance) -> Money.equal balance Money.zero)
    (Balance.to_list balances)

(* rendering *)

let payment_to_string payment =
  Printf.sprintf "%s pays %s $%s" payment.payer payment.payee
    (Money.to_string payment.amount)

let to_string = function
  | [] -> "Everyone is settled up."
  | payments -> String.concat "\n" (List.map payment_to_string payments)
