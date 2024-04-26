open! Base
open! Ppxlib

type t =
  | Type of
      { name : Longident.t loc
      ; args : t loc list
      }
  | Tuple of tuple
  | Record of record
  | Variant of { constructors : variant_constructor loc Nonempty_list.t }
  | Polymorphic_variant of
      { constructors : polymorphic_variant_constructor loc Nonempty_list.t }
  | Override of expression

and tuple = { args : t loc Nonempty_list.t }
and record = { fields : record_field loc Nonempty_list.t }

and record_field =
  { name : Longident.t loc
  ; type_ : t loc
  }

and variant_constructor =
  { name : Longident.t loc
  ; kind : [ `No_args | `Tuple of tuple | `Record of record ]
  }

and polymorphic_variant_constructor =
  | Case of
      { name : string loc
      ; arg : t loc option
      }
  | Inherit of
      { inherited_type : t loc
      ; full_parent_type : core_type
      }

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

let rec of_core_type core_type =
  let loc = { core_type.ptyp_loc with loc_ghost = true } in
  let t =
    match Attribute.get sampler_attribute core_type with
    | Some expr -> Override expr
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
       | Ptyp_tuple args -> Tuple (create_tuple args ~loc)
       | Ptyp_constr (name, args) -> Type { name; args = List.map args ~f:of_core_type }
       | Ptyp_variant (_, Open, _) -> unsupported ~loc "polymorphic variant with [>]"
       | Ptyp_variant (_, Closed, Some _) ->
         unsupported ~loc "polymorphic variant with [<]"
       | Ptyp_variant (row_fields, Closed, None) ->
         (match Nonempty_list.of_list row_fields with
          | None -> unsupported ~loc "empty polymorphic variant"
          | Some row_fields ->
            let constructors =
              Nonempty_list.map row_fields ~f:(fun row_field ->
                let loc = row_field.prf_loc in
                let constructor =
                  match row_field.prf_desc with
                  | Rtag (label, true, []) ->
                    (* TODO-someday: if all row fields don't have arguments, an
                       optimization would be to emit [Hammer.Sampler.choose] instead,
                       which might be much cheaper. *)
                    Case { name = label; arg = None }
                  | Rtag (label, false, [ arg_type ]) ->
                    Case { name = label; arg = Some (of_core_type arg_type) }
                  | Rtag (_label, true, _ :: _) | Rtag (_label, false, _ :: _ :: _) ->
                    unsupported ~loc "intersection type"
                  | Rtag (_, false, []) ->
                    invalid_syntax ~loc "invalid polymorphic variant"
                  | Rinherit inherited_type ->
                    Inherit
                      { inherited_type = of_core_type inherited_type
                      ; full_parent_type = core_type
                      }
                in
                { txt = constructor; loc })
            in
            Polymorphic_variant { constructors }))
  in
  { txt = t; loc }

and create_tuple args ~loc =
  match Nonempty_list.of_list args with
  | None -> invalid_syntax ~loc "empty tuple"
  | Some args -> { args = Nonempty_list.map args ~f:of_core_type }
;;

let create_record labels ~loc =
  match Nonempty_list.of_list labels with
  | None -> invalid_syntax ~loc "empty record"
  | Some labels ->
    let fields =
      Nonempty_list.map labels ~f:(fun label ->
        { txt =
            { name = { txt = Lident label.pld_name.txt; loc = label.pld_name.loc }
            ; type_ = of_core_type label.pld_type
            }
        ; loc = label.pld_loc
        })
    in
    { fields }
;;

let create (decl : type_declaration) =
  let loc = decl.ptype_loc in
  match decl.ptype_params with
  | [] ->
    (match decl.ptype_kind with
     | Ptype_open -> unsupported ~loc "open types"
     | Ptype_abstract ->
       (match decl.ptype_manifest with
        | None -> unsupported ~loc "abstract types"
        | Some core_type -> of_core_type core_type)
     | Ptype_record labels -> { txt = Record (create_record labels ~loc); loc }
     | Ptype_variant constructors ->
       (match Nonempty_list.of_list constructors with
        | None -> unsupported ~loc "empty variant types"
        | Some constructors ->
          let constructors =
            Nonempty_list.map constructors ~f:(fun constructor ->
              match constructor.pcd_res with
              | Some _ -> unsupported ~loc "GADTs"
              | None ->
                let loc = constructor.pcd_loc in
                let kind =
                  match constructor.pcd_args with
                  | Pcstr_tuple args ->
                    (match Nonempty_list.of_list args with
                     | None -> `No_args
                     | Some args ->
                       `Tuple { args = Nonempty_list.map args ~f:of_core_type })
                  | Pcstr_record labels -> `Record (create_record labels ~loc)
                in
                { txt =
                    { name =
                        { txt = Lident constructor.pcd_name.txt
                        ; loc = constructor.pcd_name.loc
                        }
                    ; kind
                    }
                ; loc
                })
          in
          { txt = Variant { constructors }; loc }))
  | _ -> unsupported ~loc "types with type parameters"
;;

(* TODO: remove duplication *)
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

let rec to_sampler_expression t =
  let { txt = t; loc } = t in
  let sampler_create_expr ~f =
    let state_pat, state_expr = gensym "state" ~loc in
    [%expr Hammer.Sampler.create (fun [%p state_pat] -> [%e f ~state_expr])]
  in
  match t with
  | Type { name; args } ->
    List.map args ~f:to_sampler_expression
    |> Ast_builder.Default.type_constr_conv
         ~loc
         ~f:(prefixed_type_name ~prefix:"sampler")
         name
  | Tuple tuple -> sampler_create_expr ~f:(tuple_to_expression tuple ~loc)
  | Record record -> sampler_create_expr ~f:(record_to_expression record ~loc)
  | Variant { constructors } ->
    let sampler_list_expr =
      Nonempty_list.map constructors ~f:(fun { txt = { name; kind }; loc } ->
        let constructor = Ast_builder.Default.pexp_construct ~loc name in
        match kind with
        | `No_args -> [%expr Hammer.Sampler.return [%e constructor None]]
        | `Tuple tuple ->
          sampler_create_expr ~f:(fun ~state_expr ->
            constructor (Some (tuple_to_expression tuple ~loc ~state_expr)))
        | `Record record ->
          sampler_create_expr ~f:(fun ~state_expr ->
            constructor (Some (record_to_expression record ~loc ~state_expr))))
      |> Nonempty_list.to_list
      |> Ast_builder.Default.elist ~loc
    in
    [%expr Hammer.Sampler.choose_samplers [%e sampler_list_expr]]
  | Polymorphic_variant { constructors } ->
    let sampler_list_expr =
      Nonempty_list.map constructors ~f:(fun { txt = constructor; loc } ->
        match constructor with
        | Case { name; arg = None } ->
          [%expr
            Hammer.Sampler.return [%e Ast_builder.Default.pexp_variant ~loc name.txt None]]
        | Case { name; arg = Some arg } ->
          sampler_create_expr ~f:(fun ~state_expr ->
            let sample_expr =
              [%expr Hammer.Sampler.sample [%e to_sampler_expression arg] [%e state_expr]]
            in
            Ast_builder.Default.pexp_variant ~loc name.txt (Some sample_expr))
        | Inherit { inherited_type; full_parent_type } ->
          let sampler_expr = to_sampler_expression inherited_type in
          Ast_builder.Default.pexp_coerce
            ~loc
            sampler_expr
            None
            [%type: [%t full_parent_type] Hammer.Sampler.t])
      |> Nonempty_list.to_list
      |> Ast_builder.Default.elist ~loc
    in
    [%expr Hammer.Sampler.choose_samplers [%e sampler_list_expr]]
  | Override expr -> expr

and tuple_to_expression ({ args } : tuple) ~loc ~state_expr =
  Nonempty_list.map args ~f:(fun arg ->
    let sampler = to_sampler_expression arg in
    [%expr Hammer.Sampler.sample [%e sampler] [%e state_expr]])
  |> Nonempty_list.to_list
  |> Ast_builder.Default.pexp_tuple ~loc

and record_to_expression ({ fields } : record) ~loc ~state_expr =
  let fields =
    Nonempty_list.map fields ~f:(fun { txt = { name; type_ }; loc } ->
      let sampler = to_sampler_expression type_ in
      name, [%expr Hammer.Sampler.sample [%e sampler] [%e state_expr]])
    |> Nonempty_list.to_list
  in
  Ast_builder.Default.pexp_record ~loc fields None
;;
