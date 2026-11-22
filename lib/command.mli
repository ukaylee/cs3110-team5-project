(** Commands entered by the user in the terminal. *)
type split_spec =
  | Equal_all
  | Equal of Types.person list
  | Exact of (Types.person * Money.t) list

type bill_item = {
  name : string;
  price : Money.t;
  participants : Types.person list;
}
(** Parsed item in an itemized bill. *)

type bill = {
  items : bill_item list;
  tax : Money.t;
  tip : Money.t;
}
(** Parsed itemized bill. *)

(** Commands entered by the user in the terminal.

    [Add_traveler person] adds a traveler. [Remove_traveler person] removes a
    traveler. [Add_expense (payer, category, amount, split, description)] adds
    an expense with the specified split.
    [AddBill { payer; category; bill; description }] adds an itemized bill with
    tax, tip, and item-level participants. [Edit_expense {...}] edits an
    existing expense. [Delete_expense id] deletes an expense by ID.
    [List_expenses] lists all expenses. [Balances] displays current balances.
    [Settle] calculates settlement transactions. [Set_budget (category, amount)]
    sets a category budget. [Report] generates a report. [Help] displays command
    usage. [Quit] exits the program. *)
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
  | Edit_expense of {
      id : int;
      payer : Types.person;
      category : Types.category;
      amount : Money.t;
      split : split_spec;
      description : string;
    }
  | Delete_expense of int
  | List_expenses
  | Balances
  | Settle
  | Set_budget of Types.category * Money.t
  | Report
  | Help
  | Quit

val parse : string -> (t, string) result
(** Parse a terminal command. *)
