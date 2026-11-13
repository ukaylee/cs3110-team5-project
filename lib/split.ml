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



let add_subtotal person amount subtotals =
  match List.assoc_opt person subtotals with
  | None -> (person, amount) :: subtotals
  | Some current_amount -> List.map (*look thru whole list for person :( )*)
    (fun (p, current_amount) ->
      if p = person then
        (p, Money.add current_amount amount)
      else
        (p, current_amount))
    subtotals

let calculate_subtotals bill = 
  List.fold_left
      (fun subtotals (item : item) ->
        let individual_prices =
          Money.split_evenly item.price
            (List.length item.participants)
        in
        let item_shares =
          List.combine item.participants individual_prices
        in

        List.fold_left
          (fun subtotals (person, amount) ->
            add_subtotal person amount subtotals)
          subtotals
          item_shares)
      []
      bill.items


(* let itemized :
  id:int ->
  description:string ->
  payer:Types.person ->
  category:Types.category ->
  bill:bill ->
  (Expense.t, string) result = *)
let itemized id description payer category bill =
  let subtotals = calculate_subtotals bill
  in
  Error "Not implemented yet"