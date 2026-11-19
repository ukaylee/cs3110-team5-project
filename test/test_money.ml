
open OUnit2
open Trip_tab

let money cents = Money.of_cents cents

let cents m = Money.to_cents m

let cents_list ms = List.map Money.to_cents ms

let test_zero _ =
  assert_equal 0 (cents Money.zero)

let test_of_cents _ =
  assert_equal 1250 (cents (money 1250));
  assert_equal (-1250) (cents (money (-1250)))

let test_to_cents _ =
  assert_equal 1250 (cents (money 1250));
  assert_equal (-1250) (cents (money (-1250)))

let test_to_string _ =
  assert_equal "0.00" (Money.to_string Money.zero);
  assert_equal "12.50" (Money.to_string (money 1250));
  assert_equal "12.05" (Money.to_string (money 1205));
  assert_equal "0.01" (Money.to_string (money 1));
  assert_equal "-12.50" (Money.to_string (money (-1250)));
  assert_equal "-0.01" (Money.to_string (money (-1)));
  assert_equal "-0.07" (Money.to_string (money (-7)))

let test_of_string _ =
  assert_equal (Some (money 1250)) (Money.of_string "12.50");
  assert_equal (Some (money 1250)) (Money.of_string "12.5");
  assert_equal (Some (money 1200)) (Money.of_string "12");
  assert_equal (Some (money 1205)) (Money.of_string "12.05");
  assert_equal (Some (money 1)) (Money.of_string "0.01");
  assert_equal (Some (money 0)) (Money.of_string "0");
  assert_equal (Some (money 0)) (Money.of_string "0.00");
  assert_equal (Some (money 100)) (Money.of_string "1.0")

let test_of_string_invalid _ =
  assert_equal None (Money.of_string "");
  assert_equal None (Money.of_string "-12.50");
  assert_equal None (Money.of_string "+12.50");
  assert_equal None (Money.of_string "$12.50");
  assert_equal None (Money.of_string "1,000.00");
  assert_equal None (Money.of_string "12.");
  assert_equal None (Money.of_string ".50");
  assert_equal None (Money.of_string "12.345");
  assert_equal None (Money.of_string "abc");
  assert_equal None (Money.of_string "12.5.0");
  assert_equal None (Money.of_string " 12.50");
  assert_equal None (Money.of_string "12.50 ");
  assert_equal None (Money.of_string "1234567890123")

let test_arithmetic _ =
  assert_equal 1500 (cents (Money.add (money 1000) (money 500)));
  assert_equal 500 (cents (Money.sub (money 1000) (money 500)));
  assert_equal (-500) (cents (Money.neg (money 500)));
  assert_equal 600 (cents (Money.sum [money 100; money 200; money 300]));
  assert_equal 0 (cents (Money.sum []));
  assert_equal (-100) (cents (Money.sum [money (-300); money 200]))

let test_compare _ =
  assert_equal (-1) (Money.compare (money 100) (money 200));
  assert_equal 0 (Money.compare (money 100) (money 100));
  assert_equal 1 (Money.compare (money 200) (money 100));
  assert_bool "equal values" (Money.equal (money 100) (money 100));
  assert_bool "different values"
    (not (Money.equal (money 100) (money 200)))

let test_split_evenly _ =
  assert_equal [25; 25; 25; 25]
    (cents_list (Money.split_evenly (money 100) 4));
  assert_equal [34; 33; 33]
    (cents_list (Money.split_evenly (money 100) 3));
  assert_equal [1; 1; 0]
    (cents_list (Money.split_evenly (money 2) 3));
  assert_equal [0; 0; 0]
    (cents_list (Money.split_evenly Money.zero 3));
  assert_equal [5]
    (cents_list (Money.split_evenly (money 5) 1));
  assert_equal [4; 3; 3]
    (cents_list (Money.split_evenly (money 10) 3))

let test_split_evenly_invalid _ =
  assert_raises
    (Invalid_argument "Money.split_evenly: n must be positive")
    (fun () -> Money.split_evenly (money 100) 0);
  assert_raises
    (Invalid_argument "Money.split_evenly: n must be positive")
    (fun () -> Money.split_evenly (money 100) (-1));
  assert_raises
    (Invalid_argument "Money.split_evenly: total cannot be negative")
    (fun () -> Money.split_evenly (money (-100)) 3)

let test_allocate _ =
  assert_equal [25; 25; 25; 25]
    (cents_list (Money.allocate (money 100) [1; 1; 1; 1]));
  assert_equal [50; 50]
    (cents_list (Money.allocate (money 100) [1; 1]));
  assert_equal [25; 75]
    (cents_list (Money.allocate (money 100) [1; 3]));
  assert_equal [33; 67]
    (cents_list (Money.allocate (money 100) [1; 2]));
  assert_equal [0; 0; 0]
    (cents_list (Money.allocate Money.zero [1; 2; 3]))

let test_allocate_remainders _ =
  assert_equal [34; 33; 33]
    (cents_list (Money.allocate (money 100) [1; 1; 1]));
  assert_equal [1; 0]
    (cents_list (Money.allocate (money 1) [1; 1]));
  assert_equal [0; 1; 0]
    (cents_list (Money.allocate (money 1) [1; 2; 1]))

let test_allocate_zero_weights _ =
  assert_equal [0; 100; 0]
    (cents_list (Money.allocate (money 100) [0; 1; 0]));
  assert_equal [100; 0]
    (cents_list (Money.allocate (money 100) [1; 0]))

let test_allocate_invalid _ =
  assert_raises
    (Invalid_argument "Money.allocate: total cannot be negative")
    (fun () -> Money.allocate (money (-100)) [1; 2]);
  assert_raises
    (Invalid_argument "Money.allocate: weights cannot be empty")
    (fun () -> Money.allocate (money 100) []);
  assert_raises
    (Invalid_argument "Money.allocate: weights cannot be negative")
    (fun () -> Money.allocate (money 100) [1; -1]);
  assert_raises
    (Invalid_argument "Money.allocate: weights must have a positive sum")
    (fun () -> Money.allocate (money 100) [0; 0; 0])

let suite =
  "Money Tests"
  >::: [
         "zero" >:: test_zero;
         "of_cents" >:: test_of_cents;
         "to_cents" >:: test_to_cents;
         "to_string" >:: test_to_string;
         "of_string" >:: test_of_string;
         "of_string_invalid" >:: test_of_string_invalid;
         "arithmetic" >:: test_arithmetic;
         "compare" >:: test_compare;
         "split_evenly" >:: test_split_evenly;
         "split_evenly_invalid" >:: test_split_evenly_invalid;
         "allocate" >:: test_allocate;
         "allocate_remainders" >:: test_allocate_remainders;
         "allocate_zero_weights" >:: test_allocate_zero_weights;
         "allocate_invalid" >:: test_allocate_invalid;
       ]

let () = run_test_tt_main suite
