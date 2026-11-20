(** Represents per-category and per-person spending limits. *)

type mode =
  | Soft
  | Hard

type limit = {
  amount : Money.t;
  mode : mode;
}

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
(** [empty] is the overall budget when no category or person has a spending
    limit. *)

val get_category_limit : t -> Types.category -> limit option
(** [get_category_limit budgets category] returns the complete budget setting
    details for [category], specifically its amount and mode, if one exists. *)

val get_person_limit : t -> Types.person -> limit option
(** [get_person_limit budgets person] returns the complete budget setting
    details for [person], specifically its amount and mode, if one exists. *)

val get_category_amount : t -> Types.category -> Money.t option
(** [get_category_amount budgets category] returns the budget for [category]
    according to [budgets], if one exists. Returns None if there is no budget
    for [category]. *)

val get_person_amount : t -> Types.person -> Money.t option
(** [get_person_amount budgets person] returns the budget for [person] according
    to [budgets], if one exists. Returns None if there is no budget for
    [person]. *)

val set_category : t -> Types.category -> Money.t -> mode -> (t, string) result
(** [set_category budgets category amount mode] changes the settings of
    [category] to [{amount; mode}] in [budgets]. Returns [Error] if [amount] is
    negative. *)

val set_person : t -> Types.person -> Money.t -> mode -> (t, string) result
(** [set_person budgets person amount mode] changes the settings of [person] to
    [{amount; mode}] in [budgets]. Returns [Error] if [amount] is negative.
    Requires [person] to have a nonempty name. *)

val category_limits : t -> (Types.category * limit) list
(** [category_limits budgets] returns configured category limits in
    [Types.all_categories] order. *)

val person_limits : t -> (Types.person * limit) list
(** [person_limits budgets] returns configured person limits in the order their
    budgets were first set. *)

val compare_category_spending : t -> Types.category -> Money.t -> status
(** [compare_category_spending budgets category spending] returns the budget
    status of [spending] in [category] if a limit exists for it, returns
    [Unbudgeted] if no such limit exists for [category]. *)

val compare_person_spending : t -> Types.person -> Money.t -> status
(** [compare_person_spending budgets person spending] returns the budget status
    of [spending] for [person] if a limit exists for it, returns [Unbudgeted] if
    no such limit exists for [person]. *)

val validate_hard_limits : t -> Expense.t list -> (unit, string) result
(** [validate_hard_limits budgets expenses] returns [Ok ()] if [expenses] fit
    every hard category and person limit. Otherwise, returns an error describing
    the first violation caught. Soft limits never cause an error. Affects
    whether a request for an expense is accepted or rejected. *)
