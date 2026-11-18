type t =
  | AddBill of {
      description : string;
      payer : Types.person;
      category : Types.category;
      bill : Split.bill;
    }
  | Quit
