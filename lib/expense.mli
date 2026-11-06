(** A single logged expense: who paid, how much, and who owes what share.

    This is the type every splitting method produces and every later stage
    consumes. {!Balance}, {!Budget}, {!Report} and {!Storage} see only
    [Expense.t]; they never need to know whether an expense was split equally,
    by percentage, or itemized.

    {b Invariants} (enforced by {!make}; a value of type [t] always satisfies
    them):
    - [total] is strictly positive;
    - [shares] is nonempty, lists each person at most once, and every share is
      nonnegative;
    - the shares sum to exactly [total]. *)

type t
(** A validated expense. *)

val make :
  id:int ->
  description:string ->
  payer:Types.person ->
  category:Types.category ->
  total:Money.t ->
  shares:(Types.person * Money.t) list ->
  (t, string) result
(** [make ~id ~description ~payer ~category ~total ~shares] is [Ok e] if the
    invariants above hold, and otherwise [Error msg] where [msg] says which one
    failed. The payer need not appear in [shares] (someone may pay for others).
    This module does not check that people belong to any particular trip;
    {!Trip} does that. *)

val id : t -> int
(** [id e] is the identifier [e] was made with. {!Trip} assigns unique ids. *)

val description : t -> string
(** [description e] is a short label, e.g. ["Gas station"]. *)

val payer : t -> Types.person
(** [payer e] is who paid the full [total] up front. *)

val category : t -> Types.category
(** [category e] is the spending category of [e]. *)

val total : t -> Money.t
(** [total e] is the full amount paid. *)

val shares : t -> (Types.person * Money.t) list
(** [shares e] is what each participant owes for [e], in the order given to
    {!make}. The amounts sum to [total e]. *)

val share_of : t -> Types.person -> Money.t
(** [share_of e p] is what [p] owes for [e], or {!Money.zero} if [p] did not
    take part. *)
