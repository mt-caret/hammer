open! Base
open! Ppxlib
open! Import

let sig_type_decl : (signature, rec_flag * type_declaration list) Deriving.Generator.t =
  Deriving.Generator.make_noarg (fun ~loc ~path:_ (_rec_flag, decls) ->
    let include_decl =
      mk_named_sig ~loc ~sg_name:"Hammer.S" ~handle_polymorphic_variant:true decls
      |> Option.map ~f:(fun include_info ->
        Ast_builder.Default.psig_include ~loc include_info)
    in
    Option.to_list include_decl)
;;

let lazy_sampler_symbol decl =
  let { loc; txt } = decl.ptype_name in
  let type_name = prefixed_type_name ~prefix:"sampler" txt in
  Ast_builder.Default.pvar ~loc type_name, Ast_builder.Default.evar ~loc type_name
;;

let sampler_symbol decl ~core_type =
  let { loc; txt } = decl.ptype_name in
  let type_name = prefixed_type_name ~prefix:"sampler" txt in
  let pattern = Ast_builder.Default.pvar ~loc type_name in
  Ast_builder.Default.ppat_constraint ~loc pattern core_type
;;

(* TODO: passing [wrapper] this way is horrible, and overall logic is
   overcomplicated; we need to refactor more things into data types and
   functions on them. *)
let type_variables decl ~wrapper =
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
  let core_type_for_annotation =
    let loc = decl.ptype_loc in
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
    |> wrapper
    |> Ast_builder.Default.ptyp_poly ~loc type_variable_names
  in
  ( Map.of_alist_exn (module String) type_variable_samplers
  , type_variable_patterns
  , core_type_for_annotation )
;;

let str_type_decl : (structure, rec_flag * type_declaration list) Deriving.Generator.t =
  Deriving.Generator.make_noarg (fun ~loc ~path:_ (rec_flag, decls) ->
    let rec_flag = really_recursive rec_flag decls in
    match rec_flag with
    | Nonrecursive ->
      let value_bindings =
        List.map decls ~f:(fun decl ->
          let type_variable_samplers, type_variable_patterns, core_type_for_annotation =
            type_variables decl ~wrapper:Fn.id
          in
          let sampler_pat = sampler_symbol decl ~core_type:core_type_for_annotation in
          let expr =
            Core_type.create decl ~recursive_samplers:None
            |> Core_type.to_sampler_expression ~type_variable_samplers
          in
          let expr =
            List.fold_right type_variable_patterns ~init:expr ~f:(fun pat expr ->
              [%expr fun [%p pat] -> [%e expr]])
          in
          Ast_builder.Default.value_binding ~loc:decl.ptype_loc ~pat:sampler_pat ~expr)
      in
      Ast_builder.Default.pstr_value_list ~loc Nonrecursive value_bindings
    | Recursive ->
      let decls =
        List.map decls ~f:(fun decl ->
          let lazy_sampler_pat, lazy_sampler_expr = lazy_sampler_symbol decl in
          decl, lazy_sampler_pat, [%expr force [%e lazy_sampler_expr]])
      in
      let recursive_samplers =
        match
          List.map decls ~f:(fun (decl, _lazy_sampler_pat, lazy_sampler_expr) ->
            decl.ptype_name.txt, lazy_sampler_expr)
          |> Map.of_alist (module String)
        with
        | `Ok recursive_samplers -> Some recursive_samplers
        | `Duplicate_key type_name ->
          invalid_syntax ~loc "duplicate type name: %s" type_name
      in
      let value_bindings =
        List.map decls ~f:(fun (decl, lazy_sampler_pat, _lazy_sampler_expr) ->
          let type_variable_samplers, type_variable_patterns, core_type_for_annotation =
            type_variables decl ~wrapper:(fun t -> [%type: [%t t] lazy_t])
          in
          let expr =
            Core_type.create decl ~recursive_samplers
            |> Core_type.to_sampler_expression ~type_variable_samplers
          in
          let expr =
            List.fold_right type_variable_patterns ~init:expr ~f:(fun pat expr ->
              [%expr fun [%p pat] -> [%e expr]])
          in
          let expr = [%expr lazy [%e expr]] in
          let lazy_sampler_pat =
            Ast_builder.Default.ppat_constraint
              ~loc
              lazy_sampler_pat
              core_type_for_annotation
          in
          Ast_builder.Default.value_binding
            ~loc:decl.ptype_loc
            ~pat:lazy_sampler_pat
            ~expr)
      in
      let let_bindings =
        List.map decls ~f:(fun (_decl, _lazy_sampler_pat, lazy_sampler_expr) ->
          lazy_sampler_expr)
        |> Ast_builder.Default.pexp_tuple ~loc
        |> Ast_builder.Default.pexp_let ~loc rec_flag value_bindings
      in
      let value_binding =
        Ast_builder.Default.value_binding
          ~loc
          ~pat:
            (Ast_builder.Default.ppat_tuple
               ~loc
               (List.map decls ~f:(fun (_, pat, _) -> pat)))
          ~expr:let_bindings
      in
      Ast_builder.Default.pstr_value_list ~loc Nonrecursive [ value_binding ])
;;
