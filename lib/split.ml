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



let add_person_share person amount subtotals = 
  match List.assoc_opt person subtotals with
  | None -> (person, amount) :: subtotals
  | Some current_amount -> List.map (*look thru whole list for person :( )*)
    (fun (p, current_amount) ->
      if p = person then
        (p, Money.add current_amount amount)
      else
        (p, current_amount))
    subtotals

let sum_item_shares bill = 
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
            add_person_share person amount subtotals)
          subtotals
          item_shares)
      []
      bill.items

let add_tax tax_and_tip subtotals = 
  let weights = (* figure out what percent they are of the total *)
    List.map (fun (_, amount) -> Money.to_cents amount) subtotals
  in  
  let tax_tip_shares = Money.allocate tax_and_tip weights in
  let final_shares =
  List.map2
    (fun (person, subtotal) tax ->
      (person, Money.add subtotal tax))
    subtotals
    tax_tip_shares
    in
    final_shares


(* type is itemized :
  id:int ->
  description:string ->
  payer:Types.person ->
  category:Types.category ->
  bill:bill ->
  (Expense.t, string) result = *)
let itemized id description payer category bill =
  let subtotals = sum_item_shares bill in

  let tax_and_tip = Money.add bill.tax bill.tip in

  let final_shares = add_tax tax_and_tip subtotals in

  let total = Money.sum (List.map snd final_shares) in

  Expense.make
  ~id
  ~description
  ~payer
  ~category
  ~total
  ~shares:final_shares