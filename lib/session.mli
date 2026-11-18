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