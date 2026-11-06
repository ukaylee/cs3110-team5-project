(** Amounts of money, stored as a whole number of cents.

    Amounts may be negative (a balance can be owed). The representation is
    hidden so dollars and cents are never confused. All arithmetic is exact
    integer arithmetic; no floating point is used. *)

type t
(** An amount of money in whole cents. *)

val zero : t
(** [zero] is $0.00. *)

val of_cents : int -> t
(** [of_cents c] is the amount of [c] cents. [c] may be negative. *)

val to_cents : t -> int
(** [to_cents m] is [m] as a whole number of cents. *)

val of_string : string -> t option
(** [of_string s] parses a nonnegative dollar amount such as ["12"], ["12.5"],
    or ["12.50"]. At most two digits may follow the decimal point. Returns
    [None] for anything else (signs, [$], commas, spaces, empty string, more
    than two decimal digits, or more than 12 whole-dollar digits). *)

val to_string : t -> string
(** [to_string m] formats [m] with exactly two decimal digits and no dollar sign
    or separators, e.g. ["12.50"]. A negative amount is prefixed by ['-'], e.g.
    ["-0.07"]. *)

val add : t -> t -> t
(** [add a b] is [a + b]. *)

val sub : t -> t -> t
(** [sub a b] is [a - b]. *)

val neg : t -> t
(** [neg a] is [-a]. *)

val sum : t list -> t
(** [sum ms] is the total of [ms]; [sum []] is {!zero}. *)

val compare : t -> t -> int
(** [compare a b] orders amounts by value. *)

val equal : t -> t -> bool
(** [equal a b] is true iff [a] and [b] are the same number of cents. *)

val split_evenly : t -> int -> t list
(** [split_evenly total n] divides [total] into [n] shares whose sum is exactly
    [total]. Leftover cents go one each to the {e first} shares in the list, so
    the shares differ by at most one cent and earlier positions never get less
    than later ones. For example $10.00 split 3 ways is [3.34; 3.33; 3.33].

    Requires [n > 0] and [total >= zero].
    @raise Invalid_argument if [n <= 0] or [total] is negative.

    {b Status: stub. Owner: Elizabeth.} The leftover-cent rule above is the
    team's proposal; confirm it before implementing. *)

val allocate : t -> int list -> t list
(** [allocate total weights] divides [total] in proportion to [weights] and
    returns one share per weight, in the same order, summing exactly to
    [total]. Each share is the floor of its exact proportion; remaining cents go
    one each to the shares with the largest fractional remainders, ties broken
    by earliest position. A weight of 0 always yields a share of {!zero}. Used
    for percentages and for proportional tax and tip.

    Requires [weights] nonempty, all nonnegative, with a positive sum, and
    [total >= zero].
    @raise Invalid_argument otherwise.

    {b Status: stub. Owner: Elizabeth, reviewed by Kaylee.} *)
