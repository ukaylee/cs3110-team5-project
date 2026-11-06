type t = {
  id : int;
  description : string;
  payer : Types.person;
  category : Types.category;
  total : Money.t;
  shares : (Types.person * Money.t) list;
}

let has_duplicate names =
  List.length (List.sort_uniq String.compare names) <> List.length names

let make ~id ~description ~payer ~category ~total ~shares =
  let names = List.map fst shares in
  if Money.compare total Money.zero <= 0 then
    Error "total must be greater than $0.00"
  else if shares = [] then Error "an expense needs at least one participant"
  else if has_duplicate names then Error "a person is listed more than once"
  else if
    List.exists (fun (_, m) -> Money.compare m Money.zero < 0) shares
  then Error "shares cannot be negative"
  else
    let owed = Money.sum (List.map snd shares) in
    if not (Money.equal owed total) then
      Error
        (Printf.sprintf "shares add up to $%s but the total is $%s"
           (Money.to_string owed) (Money.to_string total))
    else Ok { id; description; payer; category; total; shares }

let id e = e.id
let description e = e.description
let payer e = e.payer
let category e = e.category
let total e = e.total
let shares e = e.shares

let share_of e p =
  match List.assoc_opt p e.shares with Some m -> m | None -> Money.zero
