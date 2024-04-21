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

let unsupported ~loc fmt =
  Location.raise_errorf ~loc (Stdlib.( ^^ ) "ppx_hammer: unsupported: " fmt)
;;

let invalid_syntax ~loc fmt =
  Location.raise_errorf ~loc (Stdlib.( ^^ ) "ppx_hammer: invalid syntax: " fmt)
;;

let sampler_attribute =
  Attribute.declare
    "hammer.sampler"
    Attribute.Context.core_type
    Ast_pattern.(pstr (pstr_eval __ nil ^:: nil))
    Fn.id
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

let gensym prefix loc =
  let loc = { loc with loc_ghost = true } in
  let sym = gen_symbol ~prefix:("_" ^ prefix) () in
  Ast_builder.Default.pvar ~loc sym, Ast_builder.Default.evar ~loc sym
;;

let rec sampler_expr_of_core_type core_type =
  let loc = { core_type.ptyp_loc with loc_ghost = true } in
  match Attribute.get sampler_attribute core_type with
  | Some expr -> expr
  | None ->
    (match core_type.ptyp_desc with
     | Ptyp_any | Ptyp_var _
     | Ptyp_arrow (_, _, _)
     | Ptyp_object (_, _)
     | Ptyp_class (_, _)
     | Ptyp_alias (_, _)
     | Ptyp_poly (_, _)
     | Ptyp_package _ | Ptyp_extension _ ->
       unsupported ~loc "%s" (short_string_of_core_type core_type)
     | Ptyp_tuple _ -> failwith "TODO (Ptyp_tuple)"
     | Ptyp_constr (constr, args) ->
       List.map args ~f:sampler_expr_of_core_type
       |> Ast_builder.Default.type_constr_conv
            ~loc
            ~f:(prefixed_type_name ~prefix:"sampler")
            constr
     | Ptyp_variant (_, Open, _) -> unsupported ~loc "polymorphic variant with [>]"
     | Ptyp_variant (_, Closed, Some _) -> unsupported ~loc "polymorphic variant with [<]"
     | Ptyp_variant (row_fields, Closed, None) ->
       let row_exprs =
         List.map row_fields ~f:(fun row_field ->
           let loc = row_field.prf_loc in
           match row_field.prf_desc with
           | Rtag (label, true, []) ->
             (* TODO-someday: if all row fields don't have arguments, an
                optimization would be to emit [Hammer.Sampler.choose] instead,
                which might be much cheaper. *)
             [%expr
               Hammer.Sampler.return
                 [%e Ast_builder.Default.pexp_variant ~loc label.txt None]]
           | Rtag (label, false, [ arg_type ]) ->
             let sample_pat, sample_expr = gensym "sample" loc in
             let sampler_expr = sampler_expr_of_core_type arg_type in
             [%expr
               Hammer.Sampler.map [%e sampler_expr] ~f:(fun [%p sample_pat] ->
                 [%e Ast_builder.Default.pexp_variant ~loc label.txt (Some sample_expr)])]
           | Rtag (_label, true, _ :: _) | Rtag (_label, false, _ :: _ :: _) ->
             unsupported ~loc "intersection type"
           | Rtag (_, false, []) -> invalid_syntax ~loc "invalid polymorphic variant"
           | Rinherit inherited_type ->
             let sampler_expr = sampler_expr_of_core_type inherited_type in
             Ast_builder.Default.pexp_coerce
               ~loc
               sampler_expr
               None
               [%type: [%t core_type] Hammer.Sampler.t])
       in
       [%expr
         Hammer.Sampler.choose_samplers [%e Ast_builder.Default.elist ~loc row_exprs]])
;;

let str_type_decl : (structure, rec_flag * type_declaration list) Deriving.Generator.t =
  Deriving.Generator.make_noarg (fun ~loc ~path:_ (rec_flag, decls) ->
    match really_recursive rec_flag decls with
    | Recursive -> unsupported ~loc "recursive types"
    | Nonrecursive ->
      List.map decls ~f:(fun decl ->
        let pat =
          let { loc; txt } = decl.ptype_name in
          let pvar =
            prefixed_type_name ~prefix:"sampler" txt |> Ast_builder.Default.pvar ~loc
          in
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
          Ast_builder.Default.ppat_constraint ~loc pvar core_type
        in
        let expr =
          match decl.ptype_params with
          | [] ->
            (match decl.ptype_kind with
             | Ptype_open -> unsupported ~loc "open types"
             | Ptype_abstract ->
               (match decl.ptype_manifest with
                | None -> unsupported ~loc "abstract types"
                | Some core_type -> sampler_expr_of_core_type core_type)
             | Ptype_variant _ | Ptype_record _ ->
               (* TODO: add supports for variants and records *)
               unsupported ~loc "variant or record types")
          | _ -> unsupported ~loc "types with type parameters"
        in
        Ast_builder.Default.value_binding ~loc:decl.ptype_loc ~pat ~expr)
      |> Ast_builder.Default.pstr_value_list ~loc Nonrecursive)
;;
