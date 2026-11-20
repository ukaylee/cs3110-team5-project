(** Session state and command execution for a trip. *)

type t
(** The current trip session. *)

val create : Trip.t -> t
(** [create trip] creates a session containing [trip]. *)

val trip : t -> Trip.t
(** [trip session] returns the trip stored in [session]. *)

val add_expense : t -> Expense.t -> (t, string) result
(** [add_expense session expense] adds an expense to the trip. *)

val add_traveler : t -> Types.person -> (t, string) result
(** [add_traveler session person] adds a traveler to the trip. *)

val remove_traveler : t -> Types.person -> (t, string) result
(** [remove_traveler session person] removes a traveler from the trip. *)

val delete_expense : t -> int -> (t, string) result
(** [delete_expense session id] removes the expense with identifier [id]. *)

val edit_expense : t -> Expense.t -> (t, string) result
(** [edit_expense session expense] replaces an existing expense with the same
    identifier as [expense]. This direct operation is implemented; the
    [Command.Edit_expense] dispatch is not yet implemented. *)

val set_budget : t -> Types.category -> Money.t -> (t, string) result
(** [set_budget session category amount] sets the budget for [category]. *)

val add_bill :
  t ->
  string ->
  Types.person ->
  Types.category ->
  Split.bill ->
  (t, string) result
(** [add_bill session description payer category bill] creates an itemized
    expense from [bill] and adds it to the trip. *)

val apply : t -> Command.t -> (t * string, string) result
(** [apply session command] executes [command]. On success, returns the
    resulting session and a user-facing message; otherwise returns an error.

    TODO: Support [Command.Edit_expense] once the command supplies replacement
    expense details. Balance and settlement commands are pending integration. *)
