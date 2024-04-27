open! Base
open! Ppxlib

let sig_type_decl : (signature, rec_flag * type_declaration list) Deriving.Generator.t =
  Deriving.Generator.make_noarg (fun ~loc ~path:_ (_rec_flag, decls) ->
    let include_decl =
      mk_named_sig ~loc ~sg_name:"Hammer.S" ~handle_polymorphic_variant:true decls
      |> Option.map ~f:(fun include_info ->
        Ast_builder.Default.psig_include ~loc include_info)
    in
    Option.to_list include_decl)
;;

let prefixed_type_name ~prefix type_name =
  match type_name with
  | "t" -> prefix
  | _ -> prefix ^ "_" ^ type_name
;;

let lazy_sampler_symbol decl =
  let { loc; txt } = decl.ptype_name in
  let type_name = prefixed_type_name ~prefix:"sampler" txt in
  Ast_builder.Default.pvar ~loc type_name, Ast_builder.Default.evar ~loc type_name
;;

let sampler_symbol decl =
  let { loc; txt } = decl.ptype_name in
  let type_name = prefixed_type_name ~prefix:"sampler" txt in
  let pattern = Ast_builder.Default.pvar ~loc type_name in
  let core_type =
    let decl_type =
      Ast_builder.Default.ptyp_constr
        ~loc
        (* TODO-someday: is there a more idiomatic way to do the below? *)
        { txt = Lident decl.ptype_name.txt; loc = decl.ptype_name.loc }
        []
    in
    [%type: [%t decl_type] Hammer.Sampler.t]
  in
  Ast_builder.Default.ppat_constraint ~loc pattern core_type
;;

let invalid_syntax ~loc fmt =
  Location.raise_errorf ~loc (Stdlib.( ^^ ) "ppx_hammer: invalid syntax: " fmt)
;;

let str_type_decl : (structure, rec_flag * type_declaration list) Deriving.Generator.t =
  Deriving.Generator.make_noarg (fun ~loc ~path:_ (rec_flag, decls) ->
    let rec_flag = really_recursive rec_flag decls in
    match rec_flag with
    | Nonrecursive ->
      let value_bindings =
        List.map decls ~f:(fun decl ->
          let sampler_pat = sampler_symbol decl in
          let expr =
            Core_type.create decl ~recursive_samplers:None
            |> Core_type.to_sampler_expression
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
          let expr =
            Core_type.create decl ~recursive_samplers |> Core_type.to_sampler_expression
          in
          let expr = [%expr lazy [%e expr]] in
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
