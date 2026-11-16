type t = {
  name : string;
  travelers : Types.person list; (* in the order added *)
  expenses : Expense.t list; (* in the order added *)
  budgets : (Types.category * Money.t) list; (* in Types.all_categories order *)
  next_id : int; (* never lowered, so deleted ids are not reused *)
}

let create ~name =
  { name; travelers = []; expenses = []; budgets = []; next_id = 1 }

let name t = t.name
let travelers t = t.travelers
let expenses t = t.expenses
let next_id t = t.next_id
let is_traveler t p = List.mem p t.travelers

(* ---------- travelers ---------- *)

let add_traveler t p =
  if String.trim p = "" then Error "a traveler's name cannot be empty"
  else if is_traveler t p then
    Error (Printf.sprintf "%s is already a traveler" p)
  else Ok { t with travelers = t.travelers @ [ p ] }

(* [involves e p] is true iff [p] paid for [e] or has a share in it. *)
let involves e p =
  Expense.payer e = p || List.exists (fun (q, _) -> q = p) (Expense.shares e)

let remove_traveler t p =
  if not (is_traveler t p) then Error (Printf.sprintf "%s is not a traveler" p)
  else
    match List.find_opt (fun e -> involves e p) t.expenses with
    | Some e ->
        Error
          (Printf.sprintf
             "%s is part of expense %d (%s); delete or edit it first" p
             (Expense.id e) (Expense.description e))
    | None ->
        Ok { t with travelers = List.filter (fun q -> q <> p) t.travelers }

(* ---------- expenses ---------- *)

(* [check_people t e] is [Ok ()] iff the payer and everyone in the shares of [e]
   are travelers of [t]. *)
let check_people t e =
  let people = Expense.payer e :: List.map fst (Expense.shares e) in
  match List.find_opt (fun p -> not (is_traveler t p)) people with
  | None -> Ok ()
  | Some p -> Error (Printf.sprintf "%s is not a traveler on this trip" p)

let find_expense t id = List.find_opt (fun e -> Expense.id e = id) t.expenses

let add_expense t e =
  Result.bind (check_people t e) (fun () ->
      match find_expense t (Expense.id e) with
      | Some _ ->
          Error
            (Printf.sprintf "an expense with id %d already exists"
               (Expense.id e))
      | None ->
          Ok
            {
              t with
              expenses = t.expenses @ [ e ];
              next_id = max t.next_id (Expense.id e + 1);
            })

let edit_expense t e =
  let id = Expense.id e in
  match find_expense t id with
  | None -> Error (Printf.sprintf "there is no expense with id %d" id)
  | Some _ ->
      Result.bind (check_people t e) (fun () ->
          let replace old = if Expense.id old = id then e else old in
          Ok { t with expenses = List.map replace t.expenses })

let delete_expense t id =
  match find_expense t id with
  | None -> Error (Printf.sprintf "there is no expense with id %d" id)
  | Some _ ->
      Ok
        {
          t with
          expenses = List.filter (fun e -> Expense.id e <> id) t.expenses;
        }

(* ---------- budgets ---------- *)

let budgets t = t.budgets

let set_budget t c m =
  if Money.compare m Money.zero < 0 then Error "a budget cannot be negative"
  else
    let updated = (c, m) :: List.remove_assoc c t.budgets in
    (* Rebuild in the canonical category order. *)
    let ordered =
      List.filter_map
        (fun c' ->
          match List.assoc_opt c' updated with
          | Some amount -> Some (c', amount)
          | None -> None)
        Types.all_categories
    in
    Ok { t with budgets = ordered }
