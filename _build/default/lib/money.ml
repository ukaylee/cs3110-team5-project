type t = int

let zero = 0
let of_cents c = c
let to_cents m = m

let to_string m =
  let sign = if m < 0 then "-" else "" in
  let a = abs m in
  Printf.sprintf "%s%d.%02d" sign (a / 100) (a mod 100)

let is_digits s = s <> "" && String.for_all (fun c -> c >= '0' && c <= '9') s

let of_string s =
  match String.split_on_char '.' s with
  | [ w ] when is_digits w && String.length w <= 12 ->
      Some (int_of_string w * 100)
  | [ w; f ]
    when is_digits w && is_digits f
         && String.length w <= 12
         && String.length f <= 2 ->
      let cents =
        if String.length f = 1 then int_of_string f * 10 else int_of_string f
      in
      Some ((int_of_string w * 100) + cents)
  | _ -> None

let add = ( + )
let sub = ( - )
let neg m = -m
let sum ms = List.fold_left add zero ms
let compare = Int.compare
let equal = Int.equal
let split_evenly _total _n = failwith "TODO (Elizabeth): Money.split_evenly"
let allocate _total _weights = failwith "TODO (Elizabeth): Money.allocate"
