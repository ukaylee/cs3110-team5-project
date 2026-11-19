(** Manages the active trip and processes expenses. *)

type t

(** [create trip] creates a session containing [trip]. *)
val create : Trip.t -> t

(** [trip session] returns the trip stored in [session]. *)
val trip : t -> Trip.t

(** [add_bill session description payer category bill] creates an
    itemized expense and adds it to the current trip.

    Returns [Ok session'] containing the updated trip, or
    [Error msg] if the bill or expense is invalid. *)
val add_bill :
  t ->
  string ->
  Types.person ->
  Types.category ->
  Split.bill ->
  (t, string) result

(** [add_traveler session person] adds a traveler to the current trip. *)
val add_traveler : t -> Types.person -> (t, string) result

(** [remove_traveler session person] removes a traveler from the current trip. *)
val remove_traveler : t -> Types.person -> (t, string) result

(** [delete_expense session id] deletes the expense with the given ID. *)
val delete_expense : t -> int -> (t, string) result

(** [edit_expense session expense] replaces an existing expense. *)
val edit_expense : t -> Expense.t -> (t, string) result

(** [add_expense session expense] adds an existing expense to the
    session's trip. Returns the updated session, or an error if
    the expense cannot be added. *)
val add_expense : t -> Expense.t -> (t, string) result

(** [set_budget session category amount] updates the budget for a category. *)
val set_budget : t -> Types.category -> Money.t -> (t, string) result

(** [apply session command] executes [command] on [session].
    Returns the updated session and a message describing the result on
    success, or an error message on failure. *)
val apply : t -> Command.t -> (t * string, string) result