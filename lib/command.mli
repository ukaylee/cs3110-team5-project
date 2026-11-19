type split_spec =
  | Equal_all
  | Equal of Types.person list
  | Exact of (Types.person * Money.t) list

type bill_item = {
  name : string;
  price : Money.t;
  participants : Types.person list;
}

type bill = {
  items : bill_item list;
  tax : Money.t;
  tip : Money.t;
}

type t =
  | Add_traveler of Types.person
  | Remove_traveler of Types.person
  | Add_expense of Types.person * Types.category * Money.t * split_spec * string
  | AddBill of {
      payer : Types.person;
      category : Types.category;
      bill : bill;
      description : string;
    }
  | Edit_expense of int
  | Delete_expense of int
  | List_expenses
  | Balances
  | Settle
  | Set_budget of Types.category * Money.t
  | Report
  | Help
  | Quit

val parse : string -> (t, string) result
