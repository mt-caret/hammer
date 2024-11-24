open! Base
open! Ppxlib
open! Import

type t =
  { type_variable_samplers : (label, expression, String.comparator_witness) Map.t
  ; type_variable_patterns : pattern list
  ; core_type_for_annotation : core_type
  ; type_variable_names : label loc list
  ; ptype_loc : location
  }
[@@deriving fields ~getters]

let create decl =
  let type_variable_samplers, type_variable_patterns, core_types_and_names =
    List.map decl.ptype_params ~f:(fun (core_type, (variance, _injectivity)) ->
      let loc = core_type.ptyp_loc in
      match variance with
      | Contravariant -> unsupported ~loc "contravariant type parameters"
      | Covariant | NoVariance ->
        (match core_type.ptyp_desc with
         | Ptyp_var type_variable ->
           let name = prefixed_type_name ~prefix:"sampler" type_variable in
           let sampler_pat = Ast_builder.Default.pvar ~loc name in
           let sampler_expr = Ast_builder.Default.evar ~loc name in
           ( (type_variable, sampler_expr)
           , sampler_pat
           , (core_type, { txt = type_variable; loc }) )
         | _ ->
           invalid_syntax
             ~loc
             "unexpected %s in type variable position"
             (short_string_of_core_type core_type)))
    |> List.unzip3
  in
  let core_types, type_variable_names = List.unzip core_types_and_names in
  let loc = decl.ptype_loc in
  let core_type_for_annotation =
    let decl_type =
      Ast_builder.Default.ptyp_constr
        ~loc
        (* TODO-someday: is there a more idiomatic way to do the below? *)
        { txt = Lident decl.ptype_name.txt; loc = decl.ptype_name.loc }
        core_types
    in
    List.fold_right
      core_types
      ~init:[%type: [%t decl_type] Hammer.Sampler.t]
      ~f:(fun arg_type return_type ->
        [%type: [%t arg_type] Hammer.Sampler.t -> [%t return_type]])
  in
  { type_variable_samplers = Map.of_alist_exn (module String) type_variable_samplers
  ; type_variable_patterns
  ; core_type_for_annotation
  ; type_variable_names
  ; ptype_loc = loc
  }
;;

let core_type_for_annotation
  ({ type_variable_samplers = _
   ; type_variable_patterns = _
   ; core_type_for_annotation
   ; type_variable_names
   ; ptype_loc
   } :
    t)
  ~wrapper
  =
  wrapper core_type_for_annotation
  |> Ast_builder.Default.ptyp_poly ~loc:ptype_loc type_variable_names
;;
