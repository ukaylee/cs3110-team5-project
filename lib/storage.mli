(** Saving a trip to a text file and loading it back.

    Only the trip's {e data} is stored: its name, travelers, expenses and
    budgets. Balances, settle-up payments and reports are never stored; they are
    always recomputed from the expenses, so a saved file and the numbers on
    screen cannot disagree.

    {b File format.} A line-based text file. All money amounts are whole cents,
    so a save followed by a load is exact.

    {v
    triptab 1
    trip Weekend Getaway
    traveler Amy
    traveler Sandy
    budget dining 7000
    expense 1|Sandy|travel|4000|Gas station|Amy=1000,Sandy=1000
    v}

    - The first line is the header [triptab 1]; anything else is an error.
    - Line order: the header, one [trip] line, then [traveler] lines, then
      [budget] lines, then [expense] lines. Travelers must come before the
      expenses that mention them.
    - An [expense] line is [id|payer|category|total_cents|description|shares],
      where [shares] is a comma-separated list of [person=cents].
    - Blank lines are ignored.

    {b Validation.} Loading rebuilds the trip only through {!Trip.create},
    {!Trip.add_traveler}, {!Expense.make}, {!Trip.add_expense} and
    {!Trip.set_budget}, so every rule those enforce is checked again. A damaged
    or hand-edited file produces an [Error], never a half-built trip. Error
    messages name the line number, e.g.
    ["line 5: Zed is not a traveler on this trip"].

    {b Round trip.} For every trip [t] that {!to_string} accepts,
    [of_string (to_string t)] is a trip with the same name, the same travelers
    and expenses in the same order with the same ids and amounts, the same
    budgets, and a {!Trip.next_id} that no existing expense uses. (It may be
    lower than before if the newest expense had been deleted, because the
    counter is not stored in the file.) *)

val to_string : Trip.t -> (string, string) result
(** [to_string t] is the text form of [t] described above. It is [Error msg] if
    the name, a traveler, or an expense description contains a newline or the
    character ['|'], or if a traveler name contains [','] or ['=']; those
    characters would make the file ambiguous. *)

val of_string : string -> (Trip.t, string) result
(** [of_string s] is [Ok t] if [s] is a well-formed saved trip, and [Error msg]
    otherwise, where [msg] names the first bad line. This is the same as {!load}
    without touching the file system, which makes it easy to test. *)

val save : Trip.t -> string -> (unit, string) result
(** [save t path] writes {!to_string}[ t] to the file [path], replacing it if it
    exists. It is [Error msg] if [t] cannot be represented (see {!to_string}) or
    the file cannot be written. Nothing is written when [to_string] fails. *)

val load : string -> (Trip.t, string) result
(** [load path] reads the file [path] and returns {!of_string} of its contents.
    It is [Error msg] if the file cannot be read, is empty, or is damaged. *)
