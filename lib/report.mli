(** Produces full user-friendly readable final report of the expense exchange
    history and results within group. *)

val totals_by_category : Expense.t list -> (Types.category * Money.t) list
(** [totals_by_category expenses] returns the total monetary value of the
    [expenses] associated with each spending category. *)

val totals_by_person :
  Types.person list -> Expense.t list -> (Types.person * Money.t) list
(** [totals_by_person people expenses] returns the share of [expenses] assigned
    to each person in [people]. *)

val format_report : Types.person list -> Expense.t list -> Budget.t -> string
(** [format_report people expenses budget] returns a human-readable summary
    report of per-category spending and budgeting statuses, as well as
    per-person assigned shares of expenses. *)
