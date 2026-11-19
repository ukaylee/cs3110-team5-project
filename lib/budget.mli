(** Represents per-category spending limits. *)

type t

type status =
  | Unbudgeted
  | Within_budget of Money.t
    (* The amount of money that can be spent before the budget is met exactly *)
  | Preliminary_warning of Money.t
    (* The amount of money left to spend before hitting the budget when 85% of
       the budget has been used up *)
  | Urgent_warning of Money.t
    (* The amount of money left to spend before hitting the budget when 95% of
       the budget has been used up *)
  | Overbudget of
      Money.t (* The amount of money by which spending exceeds the budget *)

val empty : t
(** [empty] is the overall budget when no category has a spending limit. *)

val get : t -> Types.category -> Money.t option
(** [get budgets category] returns the budget for [category] according to
    [budgets], if one exists. Returns None if there is no budget for [category].
*)

val set : t -> Types.category -> Money.t -> (t, string) result
(** [set budgets category amount] sets the budget of [category] to [amount] if
    [category] exists in [budgets]. Returns the updated account of budgets for
    every category. Returns Error if [amount] is negative. *)

val compare_spending : t -> Types.category -> Money.t -> status
(** [compare_spending budgets category spending] compares the spending in a
    particular category with the budget for that same category, according to
    [budgets]. Returns [Unbudgeted] if there is no budget for [category],
    [Overbudget] if [spending] exceeds the budget for [category],
    [Urgent_warning] if at most 5% of the budget for [category] remains,
    [Preliminary_warning] if at most 15% and above 5% of the budget for
    [category] remains, and [Within_budget] otherwise. Requires
    [spending >= Money.zero]. *)
