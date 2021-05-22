open OUnit2


(* https://github.com/pdonadeo/ocaml-lens/issues/11 *)
let issue_11 _ctxt =
  let open Lens.Infix in
  let xs = [1; 2; 3; 4; 5] in
  let l = Lens.for_list 4 in
  let xs = (l -= 1) xs in
  assert_equal [1; 2; 3; 4; 4] xs

(* https://github.com/pdonadeo/ocaml-lens/issues/13 *)
[@@@warnerror "+23"]
let [@warning "-32"] issue_13 _ctxt =
  let module X = struct
    type b = {
      row : unit
    } [@@deriving lens { submodule = true; prefix = true }]
  end in
  ()
[@@@warnerror "-23"]

let suite = "Regression test" >::: [
  "issue_11"  >:: issue_11;
  "issue_13"  >:: issue_13;
]
