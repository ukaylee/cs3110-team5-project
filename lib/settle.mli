(** Settling up: turning net balances into a list of payments that brings every
    traveler's balance to zero. *)

type payment = {
  payer : Types.person;  (** the person sending money *)
  payee : Types.person;  (** the person receiving it *)
  amount : Money.t;  (** how much is sent *)
}
(** [payer] pays [payee] [amount]. Every payment produced by this module has a
    strictly positive [amount] and a [payer] different from its [payee]. *)

val greedy : Balance.t -> payment list
(** [greedy b] is a list of payments that settles [b]. {!apply}[ b (greedy b)]
    leaves everyone at zero. The person who owes the most pays the person who is
    owed the most, as much as possible (the smaller of the two amounts). This
    rule repeats to choose payments until everyone is settled. Ties go to
    whoever comes first in [b]. Payments are listed in the order they are
    chosen. *)

val apply : Balance.t -> payment list -> Balance.t
(** [apply b payments] is the balances after every payment in [payments] is
    made: each payment raises its payer's balance by its amount and lowers its
    payee's balance by the same amount. People stay in the same order as in [b].
    @raise Invalid_argument if a payment's payer or payee is not in [b]. *)

val is_settled : Balance.t -> bool
(** [is_settled b] is [true] iff every balance in [b] is zero. *)

val payment_to_string : payment -> string
(** [payment_to_string p] describes [p] as ["PAYER pays PAYEE $AMOUNT"], e.g.
    ["Amy pays Sandy $23.40"], where [AMOUNT] is formatted by
    {!Money.to_string}. *)

val to_string : payment list -> string
(** [to_string payments] is the text printed by the [settle] command: each
    payment as described by {!payment_to_string}, one per line in order, with no
    trailing newline. It is ["Everyone is settled up."] if [payments] is empty.
*)
