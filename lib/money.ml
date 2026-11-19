type t = int

let zero = 0
let of_cents c = c
let to_cents m = m

let to_string m =
  let value = Int64.of_int m in
  let negative = value < 0L in
  let magnitude =
    if negative then Int64.neg value else value
  in
  let dollars = Int64.div magnitude 100L in
  let cents = Int64.rem magnitude 100L in
  Printf.sprintf "%s%Ld.%02Ld"
    (if negative then "-" else "")
    dollars cents

let is_digits s =
  s <> ""
  && String.for_all (fun c -> c >= '0' && c <= '9') s

let of_string s =
  match String.split_on_char '.' s with
  | [w] when is_digits w && String.length w <= 12 ->
      let dollars = Int64.of_string w in
      Some (Int64.to_int (Int64.mul dollars 100L))

  | [w; f]
    when is_digits w
         && is_digits f
         && String.length w <= 12
         && String.length f <= 2 ->
      let dollars = Int64.of_string w in
      let cents =
        if String.length f = 1 then
          Int64.of_int (int_of_string f * 10)
        else
          Int64.of_int (int_of_string f)
      in
      Some
        (Int64.to_int
           (Int64.add (Int64.mul dollars 100L) cents))
  | _ -> None
let add = ( + )
let sub = ( - )
let neg m = -m
let sum ms = List.fold_left add zero ms
let compare = Int.compare
let equal = Int.equal

let split_evenly total n =
  if n <= 0 then
    invalid_arg "Money.split_evenly: n must be positive"
  else if total < 0 then
    invalid_arg "Money.split_evenly: total cannot be negative"
  else
    let quotient = total / n in
    let remainder = total mod n in
    List.init n (fun i ->
        if i < remainder then quotient + 1
        else quotient)

let allocate total weights =
  if total < 0 then
    invalid_arg "Money.allocate: total cannot be negative"
  else if weights = [] then
    invalid_arg "Money.allocate: weights cannot be empty"
  else if List.exists (fun w -> w < 0) weights then
    invalid_arg "Money.allocate: weights cannot be negative"
  else
    let weight_sum =
      List.fold_left
        (fun acc w -> Int64.add acc (Int64.of_int w))
        0L weights
    in

    if weight_sum = 0L then
      invalid_arg "Money.allocate: weights must have a positive sum"
    else
      let total64 = Int64.of_int total in

      let shares =
        List.mapi
          (fun index weight ->
            let numerator =
              Int64.mul total64 (Int64.of_int weight)
            in
            let amount =
              Int64.div numerator weight_sum
            in
            let remainder =
              Int64.rem numerator weight_sum
            in
            (index, Int64.to_int amount, remainder))
          weights
      in

      let allocated =
        List.fold_left
          (fun acc (_, amount, _) -> acc + amount)
          0 shares
      in
      let leftover = total - allocated in

      let ranked =
        List.sort
          (fun (i1, _, r1) (i2, _, r2) ->
            let cmp = Int64.compare r2 r1 in
            if cmp = 0 then Int.compare i1 i2 else cmp)
          shares
      in

      let bonus_indices =
        ranked
        |> List.mapi (fun rank (index, _, _) -> (rank, index))
        |> List.filter (fun (rank, _) -> rank < leftover)
        |> List.map snd
      in

      List.map
        (fun (index, amount, _) ->
          if List.mem index bonus_indices then amount + 1
          else amount)
        shares
