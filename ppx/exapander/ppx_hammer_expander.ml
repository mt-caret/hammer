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
     | Ptyp_variant (_, _, Some _) -> unsupported ~loc "polymorphic variant with [<]"
     | Ptyp_variant (_, _, _) -> failwith "TODO (Ptyp_variant)")
;;

let str_type_decl : (structure, rec_flag * type_declaration list) Deriving.Generator.t =
  Deriving.Generator.make_noarg (fun ~loc ~path:_ (rec_flag, decls) ->
    match really_recursive rec_flag decls with
    | Recursive -> unsupported ~loc "recursive types"
    | Nonrecursive ->
      List.map decls ~f:(fun decl ->
        let _typ =
          combinator_type_of_type_declaration decl ~f:(fun ~loc ty ->
            [%type: [%t ty] Hammer.Sampler.t])
        in
        let pat =
          let { loc; txt } = decl.ptype_name in
          let prefix = "sampler" in
          let name =
            match txt with
            | "t" -> prefix
            | _ -> prefix ^ "_" ^ txt
          in
          Ast_builder.Default.pvar ~loc name
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
               unsupported ~loc "variant or record types")
          | _ -> unsupported ~loc "types with type parameters"
        in
        Ast_builder.Default.value_binding ~loc:decl.ptype_loc ~pat ~expr)
      |> Ast_builder.Default.pstr_value_list ~loc Nonrecursive)
;;
