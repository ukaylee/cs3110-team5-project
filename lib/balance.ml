(** Net balance of expenses.*)

type t = (Types.person * Money.t) list
(** AF: [[(p1, m1); ...; (pn, mn)]] represents the balances in which person [pi]
    has net balance [mi] (positive means is owed money from others). RI: no
    person appears twice, and the [mi] sum to [Money.zero]. *)

let has_duplicates people =
  List.length (List.sort_uniq String.compare people) <> List.length people

(* computing balances *)

(* The total expense if [person] paid. *)
let amount_paid expense person =
  if Expense.payer expense = person then Expense.total expense else Money.zero

let net_balance expenses person =
  List.fold_left
    (fun acc expense ->
      Money.add acc
        (Money.sub
           (amount_paid expense person)
           (Expense.share_of expense person)))
    Money.zero expenses

(** [people_involved expense] lists everyone that [expense] mentions, the payer
    and everyone with a share. *)
let people_involved expense =
  Expense.payer expense :: List.map fst (Expense.shares expense)

let check_travelers travelers expense =
  match
    List.find_opt
      (fun person -> not (List.mem person travelers))
      (people_involved expense)
  with
  | None -> ()
  | Some person ->
      invalid_arg
        (Printf.sprintf "Balance.compute: %s in expense #%d is not a traveler"
           person (Expense.id expense))

let compute travelers expenses =
  if has_duplicates travelers then
    invalid_arg "Balance.compute: a traveler is listed more than once";
  List.iter (check_travelers travelers) expenses;
  List.map (fun person -> (person, net_balance expenses person)) travelers

(* building and reading *)

let of_list entries =
  if has_duplicates (List.map fst entries) then
    Error "a person is listed more than once"
  else
    let sum = Money.sum (List.map snd entries) in
    if Money.equal sum Money.zero then Ok entries
    else
      Error
        (Printf.sprintf "balances add up to %s cents instead of zero"
           (string_of_int (Money.to_cents sum)))

let to_list balances = balances

let net balances person =
  Option.value (List.assoc_opt person balances) ~default:Money.zero

(* rendering *)

(** [format_dollars amount] formats a nonnegative money amount as a string with
    ["$"] prepended to it. *)
let format_dollars amount = "$" ^ Money.to_string amount

let format_line (person, balance) =
  let sign = Money.compare balance Money.zero in
  if sign > 0 then
    Printf.sprintf "%s is owed %s" person (format_dollars balance)
  else if sign < 0 then
    Printf.sprintf "%s owes %s" person (format_dollars (Money.neg balance))
  else Printf.sprintf "%s is settled up" person

let to_string = function
  | [] -> "No travelers yet."
  | balances -> String.concat "\n" (List.map format_line balances)
