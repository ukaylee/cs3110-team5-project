type item = {
  name : string;
  price : Money.t;
  participants : Types.person list;
}

type bill = {
  items : item list;
  tax : Money.t;
  tip : Money.t;
}

let has_duplicate names =
  List.length (List.sort_uniq String.compare names) <> List.length names

(* validate preconditions: each item must have at least one participant, each
   item price must be non-negative, tax + tip must be non-negative, participant
   names should be valid, there should be no duplicate participant names, at
   least one item should be in the bill *)
let validate_bill bill =
  if not (List.for_all (fun item -> item.participants <> []) bill.items) then
    Error
      "each item must have at least one participant and item prices must be \
       positive and non-zero"
  else if not (bill.tax >= Money.zero && bill.tip >= Money.zero) then
    Error "tax and tip must be non-negative"
  else if
    not
      (List.for_all
         (fun item -> Money.compare item.price Money.zero >= 0)
         bill.items)
  then Error "item prices must be non-negative"
  else if
    has_duplicate
      (List.flatten (List.map (fun item -> item.participants) bill.items))
  then Error "duplicate participant names found"
  else if bill.items = [] then Error "bill must contain at least one item"
  else Ok bill

let add_person_share person amount subtotals =
  match List.assoc_opt person subtotals with
  | None -> (person, amount) :: subtotals
  | Some current_amount ->
      List.map (*look thru whole list for person :( )*)
        (fun (p, current_amount) ->
          if p = person then (p, Money.add current_amount amount)
          else (p, current_amount))
        subtotals

let sum_item_shares bill =
  List.fold_left
    (fun subtotals (item : item) ->
      let individual_prices =
        Money.split_evenly item.price (List.length item.participants)
      in
      let item_shares = List.combine item.participants individual_prices in

      List.fold_left
        (fun subtotals (person, amount) ->
          add_person_share person amount subtotals)
        subtotals item_shares)
    [] bill.items

let add_tax tax_and_tip subtotals =
  let weights =
    (* figure out what percent they are of the total *)
    List.map (fun (_, amount) -> Money.to_cents amount) subtotals
  in
  let tax_tip_shares = Money.allocate tax_and_tip weights in
  let final_shares =
    List.map2
      (fun (person, subtotal) tax -> (person, Money.add subtotal tax))
      subtotals tax_tip_shares
  in
  final_shares

(* type is itemized : id:int -> description:string -> payer:Types.person ->
   category:Types.category -> bill:bill -> (Expense.t, string) result = *)
let itemized id description payer category bill =
  if not (validate_bill bill = Ok bill) then Error "bill is invalid"
  else
    let subtotals = sum_item_shares bill in

    let tax_and_tip = Money.add bill.tax bill.tip in

    let final_shares = add_tax tax_and_tip subtotals in

    let total = Money.sum (List.map snd final_shares) in

    Expense.make ~id ~description ~payer ~category ~total ~shares:final_shares
