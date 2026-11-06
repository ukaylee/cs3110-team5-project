type person = string
type category = Travel | Lodging | Dining | Shopping | Other

let all_categories = [ Travel; Lodging; Dining; Shopping; Other ]

let category_to_string = function
  | Travel -> "travel"
  | Lodging -> "lodging"
  | Dining -> "dining"
  | Shopping -> "shopping"
  | Other -> "other"

let category_of_string s =
  let s = String.lowercase_ascii s in
  List.find_opt (fun c -> category_to_string c = s) all_categories
