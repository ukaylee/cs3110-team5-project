(** A trip: its travelers, logged expenses, and per-category budgets.

    [Trip.t] is the single piece of shared state that {!Session} threads through
    the program. It is {b immutable}: every operation that "changes" a trip
    returns a new trip and leaves the old one untouched, so a failed command can
    never leave the state half-updated.

    Operations that can fail return [(t, string) result]. The [Error] message is
    a complete, human-readable sentence fragment that {!Session} can print as
    is. This matches {!Expense.make}.

    {b Ordering guarantees} (other modules and tests rely on these):
    - {!travelers} lists people in the order they were added.
    - {!expenses} lists expenses in the order they were added.
    - {!edit_expense} keeps the edited expense in its original position.
    - {!budgets} lists categories in the order of {!Types.all_categories}.

    {b Invariants} (maintained by every operation; a value of type [t] always
    satisfies them):
    - traveler names are nonempty and pairwise distinct;
    - expense ids are pairwise distinct;
    - the payer of every expense, and every person in its shares, is a traveler;
    - every budget is nonnegative.

    {!Expense.make} deliberately does not check that people belong to a trip;
    this module is where that check lives. *)

type t
(** A trip. *)

(** Creating a trip *)

val create : name:string -> t
(** [create ~name] is a new trip called [name] with no travelers, no expenses,
    and no budgets. *)

val name : t -> string
(** [name t] is the name [t] was created with. *)

(** Travelers *)

val add_traveler : t -> Types.person -> (t, string) result
(** [add_traveler t p] is [Ok t'], where [t'] is [t] with the trimmed form of
    [p] (leading and trailing whitespace removed) added at the {e end} of
    {!travelers}. It is [Error msg] if that trimmed name is empty or is already
    a traveler. So [" Sandy"] and ["Sandy "] are the same traveler as ["Sandy"].
    Names are otherwise compared exactly, so ["amy"] and ["Amy"] are different
    travelers (see {!Types.person}). *)

val remove_traveler : t -> Types.person -> (t, string) result
(** [remove_traveler t p] is [Ok t'], where [t'] is [t] without [p]. It is
    [Error msg] if [p] is not a traveler, or if [p] is the payer of, or has a
    share in, any expense of [t]. (The caller must delete or edit those expenses
    first; this keeps the invariants above true.) *)

val travelers : t -> Types.person list
(** [travelers t] is every traveler of [t], in the order they were added. This
    includes travelers who appear in no expense. *)

val is_traveler : t -> Types.person -> bool
(** [is_traveler t p] is [true] iff [p] is a traveler of [t]. *)

(** Expenses *)

val next_id : t -> int
(** [next_id t] is an id that {!add_expense} will accept next. It differs from
    the id of every expense currently in [t] and from every id used by an
    earlier {!add_expense} on [t]'s history, so deleting an expense never causes
    its id to be handed out again. Callers build expenses with
    [Expense.make ~id:(Trip.next_id t) ...]. *)

val add_expense : t -> Expense.t -> (t, string) result
(** [add_expense t e] is [Ok t'], where [t'] is [t] with [e] added at the
    {e end} of {!expenses}. It is [Error msg] if the payer of [e] or anyone in
    its shares is not a traveler of [t], or if [t] already has an expense with
    the same id as [e]. The payer need not be among the people sharing the
    expense. *)

val edit_expense : t -> Expense.t -> (t, string) result
(** [edit_expense t e] is [Ok t'], where [t'] is [t] with the expense whose id
    equals [Expense.id e] replaced by [e], {e in the same position} in
    {!expenses}. It is [Error msg] if [t] has no expense with that id, or if the
    payer or anyone in the shares of [e] is not a traveler. *)

val delete_expense : t -> int -> (t, string) result
(** [delete_expense t id] is [Ok t'], where [t'] is [t] without the expense
    whose id is [id]. It is [Error msg] if [t] has no such expense. *)

val find_expense : t -> int -> Expense.t option
(** [find_expense t id] is the expense of [t] with id [id], or [None]. *)

val expenses : t -> Expense.t list
(** [expenses t] is every expense of [t], in the order they were added. *)

(** Budgets *)

val set_budget : t -> Types.category -> Money.t -> (t, string) result
(** [set_budget t c m] is [Ok t'], where [t'] is [t] with the budget for [c] set
    to [m], replacing any earlier budget for [c]. It is [Error msg] if [m] is
    negative. A budget of {!Money.zero} is allowed. *)

val budgets : t -> (Types.category * Money.t) list
(** [budgets t] is the budget of each category that has one, in the order of
    {!Types.all_categories}. Categories with no budget are omitted. *)

val budget_state : t -> Budget.t
(** [budget_state t] returns the category-budget state of [t]. *)
