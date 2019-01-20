open OUnit2


(* https://github.com/pdonadeo/ocaml-lens/issues/11 *)
let issue_11 ctxt =
  let open Lens.Infix in
  let xs = [1; 2; 3; 4; 5] in
  let l = Lens.for_list 4 in
  let xs = (l -= 1) xs in
  assert_equal [1; 2; 3; 4; 4] xs

let suite = "Regression test" >::: [
  "issue_11"  >:: issue_11;
]
