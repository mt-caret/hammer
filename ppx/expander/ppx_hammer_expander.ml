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

let str_type_decl : (structure, rec_flag * type_declaration list) Deriving.Generator.t =
  Deriving.Generator.make_noarg (fun ~loc ~path:_ (rec_flag, decls) ->
    let rec_flag = really_recursive rec_flag decls in
    match rec_flag with
    | Nonrecursive ->
      let value_bindings =
        List.map decls ~f:(fun decl ->
          let sampler_type = Sampler_type.create decl in
          let sampler_pat =
            sampler_symbol
              decl
              ~core_type:
                (Sampler_type.core_type_for_annotation sampler_type ~wrapper:Fn.id)
          in
          let expr =
            Core_type.create decl ~recursive_samplers:None
            |> Core_type.to_sampler_expression
                 ~type_variable_samplers:
                   (Sampler_type.type_variable_samplers sampler_type)
          in
          let expr =
            Sampler_type.type_variable_patterns sampler_type
            |> List.fold_right ~init:expr ~f:(fun pat expr ->
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
      let lazy_value_bindings =
        List.map decls ~f:(fun (decl, lazy_sampler_pat, _lazy_sampler_expr) ->
          let sampler_type = Sampler_type.create decl in
          let expr =
            Core_type.create decl ~recursive_samplers
            |> Core_type.to_sampler_expression
                 ~type_variable_samplers:
                   (Sampler_type.type_variable_samplers sampler_type)
          in
          let expr =
            Sampler_type.type_variable_patterns sampler_type
            |> List.fold_right ~init:expr ~f:(fun pat expr ->
              [%expr fun [%p pat] -> [%e expr]])
          in
          let expr = [%expr lazy [%e expr]] in
          let lazy_sampler_pat =
            Ast_builder.Default.ppat_constraint
              ~loc
              lazy_sampler_pat
              (Sampler_type.core_type_for_annotation sampler_type ~wrapper:(fun t ->
                 [%type: [%t t] lazy_t]))
          in
          Ast_builder.Default.value_binding
            ~loc:decl.ptype_loc
            ~pat:lazy_sampler_pat
            ~expr)
      in
      let forcing_value_bindings =
        List.map decls ~f:(fun (_decl, lazy_sampler_pat, lazy_sampler_expr) ->
          Ast_builder.Default.value_binding
            ~loc
            ~pat:lazy_sampler_pat
            ~expr:lazy_sampler_expr)
      in
      Ast_builder.Default.pstr_value_list ~loc rec_flag lazy_value_bindings
      @ Ast_builder.Default.pstr_value_list ~loc Nonrecursive forcing_value_bindings)
;;
