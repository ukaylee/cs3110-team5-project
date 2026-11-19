(** Commands entered by the user in the terminal. *)

type split_spec =
  | Equal_all
  | Equal of Types.person list
  | Exact of (Types.person * Money.t) list

type bill_item = {
  name : string;
  price : Money.t;
  participants : Types.person list;
}
(** Parsed item in an itemized bill. *)

type bill = {
  items : bill_item list;
  tax : Money.t;
  tip : Money.t;
}
(** Parsed itemized bill. *)

type t =
  | Add_traveler of Types.person
  | Remove_traveler of Types.person
  | Add_expense of Types.person * Types.category * Money.t * split_spec * string
  | AddBill of {
      payer : Types.person;
      category : Types.category;
      bill : bill;
      description : string;
    }
  | Edit_expense of int
  | Delete_expense of int
  | List_expenses
  | Balances
  | Settle
  | Set_budget of Types.category * Money.t
  | Report
  | Help
  | Quit

(** Parse a monetary amount. *)
let parse_money s =
  match Money.of_string s with
  | Some m -> Ok m
  | None -> Error ("Invalid monetary amount: " ^ s)

(** Parse a category. *)
let parse_category s =
  match Types.category_of_string s with
  | Some c -> Ok c
  | None -> Error ("Unknown category: " ^ s)

(** Parse an expense ID. *)
let parse_id s =
  try
    let id = int_of_string s in
    if id <= 0 then Error "Expense ID must be positive." else Ok id
  with Failure _ -> Error ("Invalid expense ID: " ^ s)

(** Split a string at its first occurrence of a character. *)
let split_first delimiter s =
  match String.index_opt s delimiter with
  | None -> (s, "")
  | Some i -> (String.sub s 0 i, String.sub s (i + 1) (String.length s - i - 1))

(** Split a string into nonempty words. *)
let clean_words s =
  String.split_on_char ' ' (String.trim s) |> List.filter (fun x -> x <> "")

(** Join words into a string. *)
let join_words = String.concat " "

(** Parse an exact split assignment such as Amy=20.00. *)
let parse_assignment s =
  let person, amount = split_first '=' s in
  if person = "" || amount = "" then
    Error ("Invalid exact split assignment: " ^ s)
  else
    match parse_money amount with
    | Ok money -> Ok (person, money)
    | Error msg -> Error msg

(** Parse a split specification.

    Supported formats: equal all equal Amy Sandy exact Amy=20.00 Sandy=15.00 *)
let parse_split tokens =
  match tokens with
  | "equal" :: "all" :: rest -> Ok (Equal_all, rest)
  | "equal" :: rest ->
      let rec take_people acc = function
        | person :: remaining -> take_people (person :: acc) remaining
        | [] ->
            if acc = [] then
              Error "Please specify at least one traveler for the split."
            else Ok (Equal (List.rev acc), [])
      in
      take_people [] rest
  | "exact" :: rest ->
      let rec take_assignments acc = function
        | assignment :: remaining -> (
            match parse_assignment assignment with
            | Ok value -> take_assignments (value :: acc) remaining
            | Error msg -> Error msg)
        | [] ->
            if acc = [] then Error "Please provide exact split assignments."
            else Ok (Exact (List.rev acc), [])
      in
      take_assignments [] rest
  | _ ->
      Error
        "Expected split format: equal all, equal NAME..., or exact \
         NAME=AMOUNT..."

(** Parse an add-expense command.

    add-expense PAYER CATEGORY AMOUNT SPLIT | DESCRIPTION *)
let parse_add_expense tokens =
  let rec split_at_pipe acc = function
    | [] -> Error "Please separate the split and description with '|'."
    | "|" :: description ->
        if List.mem "|" description then
          Error "Please use only one '|' separator."
        else Ok (List.rev acc, description)
    | token :: rest -> split_at_pipe (token :: acc) rest
  in

  match tokens with
  | payer :: category :: amount :: rest -> (
      match (parse_category category, parse_money amount) with
      | Error msg, _ | _, Error msg -> Error msg
      | Ok category, Ok amount -> (
          match split_at_pipe [] rest with
          | Error msg -> Error msg
          | Ok (split_tokens, description_words) -> (
              let description = join_words description_words in
              if description = "" then
                Error "Please provide an expense description."
              else
                match parse_split split_tokens with
                | Error msg -> Error msg
                | Ok (split, leftover) ->
                    if leftover <> [] then
                      Error "Unexpected tokens in split specification."
                    else
                      Ok
                        (Add_expense
                           (payer, category, amount, split, description)))))
  | _ -> Error "Usage: add-expense PAYER CATEGORY AMOUNT SPLIT | DESCRIPTION"

(** Parse a bill item.

    Format: ITEM NAME PRICE PERSON1, PERSON2

    The item name can contain multiple words. Participants are separated by
    commas. *)
let parse_bill_item s =
  let words = clean_words s in

  let rec search prefix remaining =
    match remaining with
    | [] -> Error "Missing item price."
    | price_word :: rest -> (
        match parse_money price_word with
        | Error _ -> search (prefix @ [ price_word ]) rest
        | Ok price ->
            let name = join_words prefix in
            let participant_text = join_words rest in

            if name = "" then Error "Please provide an item name."
            else if participant_text = "" then
              Error "Please specify item participants."
            else
              let participants =
                String.split_on_char ',' participant_text
                |> List.map String.trim
              in

              if List.exists (fun p -> p = "") participants then
                Error "Invalid participant list."
              else Ok { name; price; participants })
  in

  search [] words

(** Parse an add-bill command.

    add-bill PAYER CATEGORY tax=X tip=Y | DESCRIPTION | ITEM PRICE PEOPLE | ITEM
    PRICE PEOPLE *)
let parse_add_bill input =
  let sections = String.split_on_char '|' input |> List.map String.trim in

  match sections with
  | header :: description :: item_sections when item_sections <> [] -> (
      match clean_words header with
      | [ "add-bill"; payer; category; tax_arg; tip_arg ] -> (
          let tax_name, tax_amount = split_first '=' tax_arg in
          let tip_name, tip_amount = split_first '=' tip_arg in

          if tax_name <> "tax" || tip_name <> "tip" then
            Error "Expected tax=X and tip=Y."
          else
            match
              ( parse_category category,
                parse_money tax_amount,
                parse_money tip_amount )
            with
            | Error msg, _, _ | _, Error msg, _ | _, _, Error msg -> Error msg
            | Ok category, Ok tax, Ok tip -> (
                let rec parse_items acc = function
                  | [] -> Ok (List.rev acc)
                  | section :: rest -> (
                      match parse_bill_item section with
                      | Error msg -> Error msg
                      | Ok item -> parse_items (item :: acc) rest)
                in

                match parse_items [] item_sections with
                | Error msg -> Error msg
                | Ok items ->
                    if description = "" then
                      Error "Please provide a bill description."
                    else
                      Ok
                        (AddBill
                           {
                             description;
                             payer;
                             category;
                             bill = { items; tax; tip };
                           })))
      | _ ->
          Error
            "Usage: add-bill PAYER CATEGORY tax=X tip=Y | DESCRIPTION | ITEM \
             PRICE PEOPLE")
  | _ ->
      Error
        "Usage: add-bill PAYER CATEGORY tax=X tip=Y | DESCRIPTION | ITEM PRICE \
         PEOPLE"

(** Parse a terminal command. *)
let parse input =
  let input = String.trim input in
  let tokens = clean_words input in

  match tokens with
  | [] -> Error "Please enter a command."
  | [ "help" ] -> Ok Help
  | [ "quit" ] -> Ok Quit
  | [ "expenses" ] -> Ok List_expenses
  | [ "balances" ] -> Ok Balances
  | [ "settle" ] -> Ok Settle
  | [ "report" ] -> Ok Report
  | "add-traveler" :: [ person ] -> Ok (Add_traveler person)
  | "remove-traveler" :: [ person ] -> Ok (Remove_traveler person)
  | "edit-expense" :: [ id ] -> (
      match parse_id id with
      | Ok id -> Ok (Edit_expense id)
      | Error msg -> Error msg)
  | "delete-expense" :: [ id ] -> (
      match parse_id id with
      | Ok id -> Ok (Delete_expense id)
      | Error msg -> Error msg)
  | [ "budget"; category; amount ] -> (
      match (parse_category category, parse_money amount) with
      | Ok category, Ok amount -> Ok (Set_budget (category, amount))
      | Error msg, _ | _, Error msg -> Error msg)
  | "add-expense" :: rest -> parse_add_expense rest
  | "add-bill" :: _ -> parse_add_bill input
  | _ -> Error "Unrecognized command. Type 'help' for available commands."
