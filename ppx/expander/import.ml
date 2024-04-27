open! Base
open! Ppxlib

let unsupported ~loc fmt =
  Location.raise_errorf ~loc (Stdlib.( ^^ ) "ppx_hammer: unsupported: " fmt)
;;

let invalid_syntax ~loc fmt =
  Location.raise_errorf ~loc (Stdlib.( ^^ ) "ppx_hammer: invalid syntax: " fmt)
;;

(* Taken from ppx_quickcheck *)
let short_string_of_core_type core_type =
  match core_type.ptyp_desc with
  | Ptyp_any -> "wildcard type"
  | Ptyp_var _ -> "type variable"
  | Ptyp_arrow _ -> "function type"
  | Ptyp_tuple _ -> "tuple type"
  | Ptyp_constr _ -> "type name"
  | Ptyp_object _ -> "object type"
  | Ptyp_class _ -> "class type"
  | Ptyp_alias _ -> "type variable alias"
  | Ptyp_variant _ -> "polymorphic variant"
  | Ptyp_poly _ -> "explicit polymorphic type"
  | Ptyp_package _ -> "first-class module type"
  | Ptyp_extension _ -> "ppx extension type"
;;

let prefixed_type_name ~prefix type_name =
  match type_name with
  | "t" -> prefix
  | _ -> prefix ^ "_" ^ type_name
;;

let gensym prefix ~loc =
  let loc = { loc with loc_ghost = true } in
  let sym = gen_symbol ~prefix:("_" ^ prefix) () in
  Ast_builder.Default.pvar ~loc sym, Ast_builder.Default.evar ~loc sym
;;
