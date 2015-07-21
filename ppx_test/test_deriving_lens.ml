open OUnit2

let set lens = Lens.(lens.set)
let get lens = Lens.(lens.get)

module M : sig
  type car = {
    make : string;
    model: string;
    mileage: int;
  } [@@deriving lens]
end = struct
  type car = {
    make : string;
    model: string;
    mileage: int;
  } [@@deriving lens]
end

let test_basic ctxt = M.(
  let car = { make = "Citroën"; model = "2CV"; mileage = 1948 } in

  assert_equal (get car_model car) "2CV";

  assert_equal (set car_model "deux chevaux" car)
               { make = "Citroën"; model = "deux chevaux"; mileage = 1948 }
)

let suite = "Test deriving(lens)" >::: [
    "test_basic"    >:: test_basic;
  ]
