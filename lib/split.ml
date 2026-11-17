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

module PersonMap = Map.Make (String)
(*so we can use a dictionary (map) for list of people*)

(* helper for the validation function *)
let has_duplicate names =
  List.length (List.sort_uniq String.compare names) <> List.length names

(* validate preconditions: each item must have at least one participant, each
   item price must be non-negative, tax + tip must be non-negative, participant
   names should be valid, there should be no duplicate participant names, at
   least one item should be in the bill *)
let validate_bill bill =
  if bill.items = [] then Error "bill must contain at least one item"
  else if not (List.for_all (fun item -> item.participants <> []) bill.items)
  then Error "each item must have at least one participant"
  else if
    not
      (List.for_all
         (fun item -> Money.compare item.price Money.zero >= 0)
         bill.items)
  then Error "item prices must be non-negative"
  else if
    Money.compare bill.tax Money.zero < 0
    || Money.compare bill.tip Money.zero < 0
  then Error "tax and tip must be non-negative"
  else if List.exists (fun item -> has_duplicate item.participants) bill.items
  then Error "duplicate participants found within an item"
  else if
    Money.compare
      (Money.sum (List.map (fun item -> item.price) bill.items))
      Money.zero
    = 0
  then Error "total item subtotal must be greater than zero"
  else Ok bill

let add_person_share person amount subtotals =
  match PersonMap.find_opt person subtotals with
  | None -> PersonMap.add person amount subtotals
  | Some current_amount ->
      PersonMap.add person (Money.add current_amount amount) subtotals

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
    PersonMap.empty bill.items

let allocate_tax_tip tax_and_tip subtotals =
  let weights =
    (* figure out what percent they are of the total *)
    (*also convert from person map to a list because expense expects a list 
    and we don't need to use the map's lookup anymore*)
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
  match validate_bill bill with
  | Error msg -> Error msg
  | Ok bill ->
      let subtotals = PersonMap.bindings (sum_item_shares bill) in
      (*the result is a person map with key person and value money owed. we used
        a map in the function because it makes look up easier, but now we can
        convert to a list because we don't need to look up anymore and the
        expense type expects a list.*)

      let tax_and_tip = Money.add bill.tax bill.tip in

      let final_shares = allocate_tax_tip tax_and_tip subtotals in

      let total = Money.sum (List.map snd final_shares) in

      Expense.make ~id ~description ~payer ~category ~total ~shares:final_shares
