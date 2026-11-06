(** Shared vocabulary used by every module: travelers and spending categories. *)

type person = string
(** A traveler, identified by name. Names are compared exactly, so ["amy"] and
    ["Amy"] are different people. {!Trip} is responsible for ensuring that
    names are nonempty and unique within a trip. *)

(** The fixed spending categories that budgets and reports are keyed by. *)
type category =
  | Travel
  | Lodging
  | Dining
  | Shopping
  | Other

val all_categories : category list
(** [all_categories] lists every category once, in the order reports should
    display them. *)

val category_to_string : category -> string
(** [category_to_string c] is the lowercase name of [c], e.g. ["dining"]. *)

val category_of_string : string -> category option
(** [category_of_string s] is the category named [s] (case-insensitive), or
    [None] if there is none. It is the inverse of {!category_to_string}. *)
