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

module M_with_type_named_t : sig
  type t = {
    make : string;
    model: string;
    mileage: int;
  } [@@deriving lens]
end = struct
  type t = {
    make : string;
    model: string;
    mileage: int;
  } [@@deriving lens]
end

let test_with_type_named_t ctxt = M_with_type_named_t.(
  let car = { make = "Citroën"; model = "2CV"; mileage = 1948 } in

  assert_equal (get model car) "2CV";

  assert_equal (set model "deux chevaux" car)
               { make = "Citroën"; model = "deux chevaux"; mileage = 1948 }
)

module M_with_prefix : sig
  type car = {
    make : string;
    model: string;
    mileage: int;
  } [@@deriving lens { prefix = true }]
end = struct
  type car = {
    make : string;
    model: string;
    mileage: int;
  } [@@deriving lens { prefix = true }]
end

let test_prefix ctxt = M_with_prefix.(
  let car = { make = "Citroën"; model = "2CV"; mileage = 1948 } in

  assert_equal (get lens_car_model car) "2CV";

  assert_equal (set lens_car_model "deux chevaux" car)
               { make = "Citroën"; model = "deux chevaux"; mileage = 1948 }
)

let suite = "Test deriving(lens)" >::: [
    "test_basic"             >:: test_basic;
    "test_with_type_named_t" >:: test_with_type_named_t;
    "test_prefix"            >:: test_prefix;
  ]
