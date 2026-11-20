(** An individual item on a bill, including its name, price, and the
    participants responsible for splitting its cost. *)
type item = {
  name : string;
  price : Money.t;
  participants : Types.person list;
}

(** An itemized bill containing a list of items and the tax and tip
    amounts to distribute among participants. *)
type bill = {
  items : item list;
  tax : Money.t;
  tip : Money.t;
}

(** [itemized id description payer category bill] creates an expense
    from an itemized bill. Each item's cost is split evenly among its
    participants, and each person's shares are summed to calculate
    their subtotal. Tax and tip are then allocated proportionally
    based on these subtotals.

    [id], [description], [payer], and [category] specify the expense's
    information. [bill] contains the items, tax, and tip.

    Returns [Ok expense] if the bill is valid and the expense is
    successfully created, or [Error msg] if the bill or resulting
    expense is invalid. *)
val itemized :
  int ->
  string ->
  Types.person ->
  Types.category ->
  bill ->
  (Expense.t, string) result