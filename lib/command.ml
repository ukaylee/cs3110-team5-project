type t =
  | AddBill of {
      id : int;
      description : string;
      payer : Types.person;
      category : Types.category;
      bill : Split.bill;
    }
  | Quit
