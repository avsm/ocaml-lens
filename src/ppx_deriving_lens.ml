open Longident
open Location
open Asttypes
open Parsetree
open Ast_helper
open Ast_convenience

let deriver = "lens"
let raise_errorf = Ppx_deriving.raise_errorf

type lens_options = {
  prefix: bool;
  submodule: bool;
}

let lens_default_options = {
  prefix = false;
  submodule = false;
}

let bool_option deriver name expr =
  match expr with
  | [%expr true] |  [%expr "true"]  -> true
  | [%expr false] | [%expr "false"] -> false
  | _ -> raise_errorf ~loc:expr.pexp_loc "%s %s option must be either true or false" deriver name

let parse_options options =
  options |> List.fold_left (fun deriver_options (name, expr) ->
    match name with
    | "prefix" | "affix" -> { deriver_options with prefix = bool_option deriver name expr }
    | "submodule" ->        { deriver_options with submodule = bool_option deriver name expr }
    | _ -> raise_errorf ~loc:expr.pexp_loc "%s does not support option %s" deriver name
  ) lens_default_options

(* builds the expression: { record with field = value } *)
let updated_record record field value =
  Exp.mk (
    Pexp_record (
      [ (mknoloc (Lident field), Exp.mk (Pexp_ident (mknoloc (Lident value)))) ],
      Some (Exp.mk (Pexp_ident (mknoloc (Lident record))))
    )
  )

(* wraps a list of signatures into a module signature *)
let declare_module loc module_name signatures =
  {psig_desc = Psig_module
    {pmd_name = {txt = module_name; loc};
     pmd_type = {pmty_desc = Pmty_signature signatures; pmty_loc = loc; pmty_attributes = [] };
     pmd_loc = loc;
     pmd_attributes = []};
  psig_loc = loc}

(* wraps a list of expression into a module *)
let define_module loc module_name expressions =
  let expressions = List.map (fun x -> {pstr_desc = Pstr_value (Nonrecursive,[x]); pstr_loc = loc}) expressions in
  Pstr_module {
    pmb_name = {txt = module_name; loc};
    pmb_expr = {pmod_desc = Pmod_structure expressions; pmod_loc = loc; pmod_attributes = []};
    pmb_loc = loc;
    pmb_attributes = [];
  }

let lens_name ~deriver_options record_type_decl field_name =
  if deriver_options.submodule
  then
    field_name
  else
    if deriver_options.prefix
    then Ppx_deriving.mangle_type_decl (`PrefixSuffix (deriver,field_name)) record_type_decl
    else Ppx_deriving.mangle_type_decl (`Suffix field_name) record_type_decl

let module_name  ~deriver_options { ptype_name = { txt = name } } =
  if deriver_options.prefix
  then (String.capitalize_ascii name) ^ "Lens"
  else "Lens"

let wrap_in_submodule_sig ~deriver_options record loc signatures =
  if deriver_options.submodule
  then
    let module_name = module_name  ~deriver_options record in
    [declare_module loc module_name signatures]
  else signatures

let wrap_in_submodule_struct ~deriver_options record loc expressions =
  if deriver_options.submodule
  then
    let module_name = module_name  ~deriver_options record in
    {pstr_desc = define_module loc module_name expressions; pstr_loc = loc}
  else {pstr_desc = Pstr_value (Nonrecursive, expressions); pstr_loc = loc}

let str_of_type ~options ~path ({ ptype_loc = loc } as type_decl) =
  let deriver_options = parse_options options in
  match type_decl.ptype_kind with
  | Ptype_record labels -> labels
    |> List.map (fun { pld_name = { txt = name; loc } } ->
      name, [%expr Lens.{
        get = (fun r -> [%e Exp.field (evar "r") (mknoloc (Lident name))] );
        set = (fun v r -> [%e updated_record "r" name "v"]);
      }]
    )
    |> List.map (fun (name,lens) ->
      Vb.mk (pvar (lens_name ~deriver_options type_decl name)) lens
    )
    |> wrap_in_submodule_struct ~deriver_options type_decl loc
  | _ -> raise_errorf ~loc "%s can be derived only for record types" deriver

let type_named name =
  Typ.mk (Ptyp_constr (mknoloc (Lident name), []))

let sig_of_type ~options ~path ({ ptype_loc = loc; ptype_name = { txt = record_name } } as type_decl) =
  let deriver_options = parse_options options in
  match type_decl.ptype_kind with
  | Ptype_record labels -> labels
    |> List.map (fun { pld_name = { txt = name; loc }; pld_type } ->
      let lens_type = [%type: ([%t type_named record_name], [%t pld_type]) Lens.t] in
      Sig.value (Val.mk (mknoloc (lens_name ~deriver_options type_decl name)) lens_type)
    )
    |> wrap_in_submodule_sig ~deriver_options type_decl loc
  | _ -> raise_errorf ~loc "%s can be derived only for record types" deriver

let () =
  Ppx_deriving.(register (create deriver
    ~type_decl_str: (fun ~options ~path type_decls ->
       List.map (str_of_type ~options ~path) type_decls)
    ~type_decl_sig: (fun ~options ~path type_decls ->
       List.concat (List.map (sig_of_type ~options ~path) type_decls))
    ()
  ))
