(** Manages the current trip and applies user commands. *)

type t

val create : Trip.t -> t
(** [create trip] creates a session with [trip] as its active trip. *)

val trip : t -> Trip.t
(** [trip session] returns the current trip. *)

(** Applies commands to the current trip. *)

val apply : Trip.t -> Command.t -> (Trip.t, string) result