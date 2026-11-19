(** Net balance of expenses.*)

type t
(** Represents the net balance for each individual traveler. Each person appears
    at most once, and the balances sum to exactly zero. *)

val compute : Types.person list -> Expense.t list -> t
(** [compute travelers expenses] is the net balance of every person in
    [travelers], in that order. A person's balance is the total of the expenses
    they paid minus the sum of their shares across [expenses]. Travelers who
    appear in no expense have a balance of {!Money.zero}.

    Requires: [travelers] has no duplicates, and the payer and every person in
    the shares of each expense is in [travelers].
    @raise Invalid_argument if the requirements do not hold. *)

val of_list : (Types.person * Money.t) list -> (t, string) result
(** [of_list entries] is [Ok b], where [b] has exactly the balances in [entries]
    in the same order. It is [Error msg] if a person appears more than once or
    the amounts do not sum to zero. *)

val to_list : t -> (Types.person * Money.t) list
(** [to_list b] is every person in [b] with their balance, in the order [b] was
    built with. *)

val net : t -> Types.person -> Money.t
(** [net b p] is [p]'s balance in [b], or zero if [p] is not in [b]. *)

val to_string : t -> string
(** [to_string b] is the text printed by the [balances] command: one line per
    person in the order of [b] describing how much money they owe or are owed.
    Returns ["No travelers yet."] if [b] has no people. *)
