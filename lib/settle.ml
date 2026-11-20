(** Turns net balances into payments with an optimized settlement order. *)

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

(* fewest payments *)

(** [max_searched] is the largest number of people with a nonzero balance for
    which {!payments} searches for the fewest payments. *)
let max_searched = 12

(** [sum_of entries] is the total of the balances in [entries]. *)
let sum_of entries = Money.sum (List.map snd entries)

(** [is_zero_sum entries] is [true] iff the balances in [entries] sum to zero.
*)
let is_zero_sum entries = Money.equal (sum_of entries) Money.zero

(** [choices xs] is every way to pick some of the elements of [xs], as pairs
    [(picked, left)]. In each pair, every element of [xs] is in exactly one of
    [picked] and [left], and both keep the order of [xs]. There are [2^n] pairs
    if [xs] has [n] elements, starting with [([], xs)]. *)
let rec choices = function
  | [] -> [ ([], []) ]
  | x :: rest ->
      let without_x = List.map (fun (picked, left) -> (picked, x :: left)) in
      let with_x = List.map (fun (picked, left) -> (x :: picked, left)) in
      let rest_choices = choices rest in
      without_x rest_choices @ with_x rest_choices

(** [most_groups candidates] is the element of [candidates] with the most
    groups; on a tie, the earliest one. It is [[]] if [candidates] is empty. *)
let most_groups candidates =
  List.fold_left
    (fun best candidate ->
      if List.length candidate > List.length best then candidate else best)
    [] candidates

(** [best_groups entries] splits [entries] into as many groups as possible whose
    balances each sum to zero. Every entry is in exactly one group, and each
    group keeps the order of [entries]. It is [[]] if [entries] is empty.
    Requires: [entries] sums to zero, has no zero balances, and lists each
    person once. *)
let rec best_groups entries =
  match entries with
  | [] -> []
  | ((_, first_balance) as first) :: rest -> (
      let cancels (_, balance) =
        Money.equal (Money.add first_balance balance) Money.zero
      in
      match List.find_opt cancels rest with
      | Some ((partner_person, _) as partner) ->
          (* Shortcut: two people who exactly cancel are always one group in
             some best split, so no need to search. *)
          let left =
            List.filter (fun (person, _) -> person <> partner_person) rest
          in
          [ first; partner ] :: best_groups left
      | None ->
          choices rest
          |> List.filter (fun (picked, _) -> is_zero_sum (first :: picked))
          |> List.map (fun (picked, left) ->
              (first :: picked) :: best_groups left)
          |> most_groups)

(** [settle_group group] is greedy on the balances in [group]. Requires: [group]
    sums to zero and lists each person once. *)
let settle_group group =
  match Balance.of_list group with
  | Ok balances -> greedy balances
  | Error msg -> failwith ("Settle.payments: impossible: " ^ msg)

let payments balances =
  let owing_or_owed =
    List.filter
      (fun (_, balance) -> not (Money.equal balance Money.zero))
      (Balance.to_list balances)
  in
  if List.length owing_or_owed > max_searched then greedy balances
  else List.concat_map settle_group (best_groups owing_or_owed)

(* applying payments *)

(** [check_person balances person] is [()] if [person] is in [balances].
    @raise Invalid_argument otherwise. *)
let check_person balances person =
  if not (List.mem_assoc person (Balance.to_list balances)) then
    invalid_arg
      (Printf.sprintf "Settle.apply: %s is not in the balances" person)

(** [check_payment balances payment] is [()] if [payment] is valid for
    [balances]: its payer and payee are both in [balances], they are different
    people, and its amount is strictly positive.
    @raise Invalid_argument otherwise. *)
let check_payment balances payment =
  check_person balances payment.payer;
  check_person balances payment.payee;
  if payment.payer = payment.payee then
    invalid_arg
      (Printf.sprintf "Settle.apply: %s cannot pay themselves" payment.payer);
  if Money.compare payment.amount Money.zero <= 0 then
    invalid_arg
      (Printf.sprintf "Settle.apply: payment from %s to %s must be positive"
         payment.payer payment.payee)

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
  List.iter (check_payment balances) payments;
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
