open! Core
open Hammer.Sampler
module Parsetree = Astlib.Ast_501.Parsetree

module Location = struct
  let sexp_of_lexing_position = Source_code_position.sexp_of_t

  type lexing_position = Source_code_position.t =
    { pos_fname : string
    ; pos_lnum : int
    ; pos_bol : int
    ; pos_cnum : int
    }
  [@@deriving hammer]

  type t = Astlib.Location.t =
    { loc_start : lexing_position
    ; loc_end : lexing_position
    ; loc_ghost : bool
    }
  [@@deriving sexp_of, hammer]

  type 'a loc = 'a Astlib.Location.loc
end

module Longident = struct
  type t = Astlib.Longident.t =
    | Lident of string
    | Ldot of t * string
    | Lapply of t * t
  [@@deriving sexp_of, hammer]
end

module Asttypes = struct
  type constant = Astlib.Ast_501.Asttypes.constant =
    | Const_int of int
    | Const_char of char
    | Const_string of string * Location.t * string option
    | Const_float of string
    | Const_int32 of int32
    | Const_int64 of int64
    | Const_nativeint of nativeint
  [@@deriving sexp_of, hammer]

  type rec_flag = Astlib.Ast_501.Asttypes.rec_flag =
    | Nonrecursive
    | Recursive
  [@@deriving sexp_of, hammer]

  type direction_flag = Astlib.Ast_501.Asttypes.direction_flag =
    | Upto
    | Downto
  [@@deriving sexp_of, hammer]

  type private_flag = Astlib.Ast_501.Asttypes.private_flag =
    | Private
    | Public
  [@@deriving sexp_of, hammer]

  type mutable_flag = Astlib.Ast_501.Asttypes.mutable_flag =
    | Immutable
    | Mutable
  [@@deriving sexp_of, hammer]

  type virtual_flag = Astlib.Ast_501.Asttypes.virtual_flag =
    | Virtual
    | Concrete
  [@@deriving sexp_of, hammer]

  type override_flag = Astlib.Ast_501.Asttypes.override_flag =
    | Override
    | Fresh
  [@@deriving sexp_of, hammer]

  type closed_flag = Astlib.Ast_501.Asttypes.closed_flag =
    | Closed
    | Open
  [@@deriving sexp_of, hammer]

  type label = string [@@deriving sexp_of, hammer]

  type arg_label = Astlib.Ast_501.Asttypes.arg_label =
    | Nolabel
    | Labelled of string
    | Optional of string
  [@@deriving sexp_of, hammer]

  type 'a loc = 'a Astlib.Ast_501.Asttypes.loc =
    { txt : 'a
    ; loc : Location.t
    }
  [@@deriving sexp_of, hammer]

  type variance = Astlib.Ast_501.Asttypes.variance =
    | Covariant
    | Contravariant
    | NoVariance
  [@@deriving sexp_of, hammer]

  type injectivity = Astlib.Ast_501.Asttypes.injectivity =
    | Injective
    | NoInjectivity
  [@@deriving sexp_of, hammer]
end

type constant = Astlib.Ast_501.Parsetree.constant =
  | Pconst_integer of string * char option
  | Pconst_char of char
  | Pconst_string of string * Location.t * string option
  | Pconst_float of string * char option
[@@deriving sexp_of, hammer]

type location_stack = Location.t list [@@deriving sexp_of, hammer]

type attribute = Astlib.Ast_501.Parsetree.attribute =
  { attr_name : string Asttypes.loc
  ; attr_payload : payload
  ; attr_loc : Location.t
  }

and extension = string Asttypes.loc * payload
and attributes = attribute list

and payload = Astlib.Ast_501.Parsetree.payload =
  | PStr of structure
  | PSig of signature
  | PTyp of core_type
  | PPat of pattern * expression option

and core_type = Astlib.Ast_501.Parsetree.core_type =
  { ptyp_desc : core_type_desc
  ; ptyp_loc : Location.t
  ; ptyp_loc_stack : location_stack
  ; ptyp_attributes : attributes
  }

and core_type_desc = Astlib.Ast_501.Parsetree.core_type_desc =
  | Ptyp_any
  | Ptyp_var of string
  | Ptyp_arrow of Asttypes.arg_label * core_type * core_type
  | Ptyp_tuple of core_type list
  | Ptyp_constr of Longident.t Asttypes.loc * core_type list
  | Ptyp_object of object_field list * Asttypes.closed_flag
  | Ptyp_class of Longident.t Asttypes.loc * core_type list
  | Ptyp_alias of core_type * string
  | Ptyp_variant of row_field list * Asttypes.closed_flag * string list option
  | Ptyp_poly of string Asttypes.loc list * core_type
  | Ptyp_package of package_type
  | Ptyp_extension of extension

and package_type = Longident.t Asttypes.loc * (Longident.t Asttypes.loc * core_type) list

and row_field = Astlib.Ast_501.Parsetree.row_field =
  { prf_desc : row_field_desc
  ; prf_loc : Location.t
  ; prf_attributes : attributes
  }

and row_field_desc = Astlib.Ast_501.Parsetree.row_field_desc =
  | Rtag of string Asttypes.loc * bool * core_type list
  | Rinherit of core_type

and object_field = Astlib.Ast_501.Parsetree.object_field =
  { pof_desc : object_field_desc
  ; pof_loc : Location.t
  ; pof_attributes : attributes
  }

and object_field_desc = Astlib.Ast_501.Parsetree.object_field_desc =
  | Otag of string Asttypes.loc * core_type
  | Oinherit of core_type

and pattern = Astlib.Ast_501.Parsetree.pattern =
  { ppat_desc : pattern_desc
  ; ppat_loc : Location.t
  ; ppat_loc_stack : location_stack
  ; ppat_attributes : attributes
  }

and pattern_desc = Astlib.Ast_501.Parsetree.pattern_desc =
  | Ppat_any
  | Ppat_var of string Asttypes.loc
  | Ppat_alias of pattern * string Asttypes.loc
  | Ppat_constant of constant
  | Ppat_interval of constant * constant
  | Ppat_tuple of pattern list
  | Ppat_construct of
      Longident.t Asttypes.loc * (string Asttypes.loc list * pattern) option
  | Ppat_variant of string * pattern option
  | Ppat_record of (Longident.t Asttypes.loc * pattern) list * Asttypes.closed_flag
  | Ppat_array of pattern list
  | Ppat_or of pattern * pattern
  | Ppat_constraint of pattern * core_type
  | Ppat_type of Longident.t Asttypes.loc
  | Ppat_lazy of pattern
  | Ppat_unpack of string option Asttypes.loc
  | Ppat_exception of pattern
  | Ppat_extension of extension
  | Ppat_open of Longident.t Asttypes.loc * pattern

and expression = Astlib.Ast_501.Parsetree.expression =
  { pexp_desc : expression_desc
  ; pexp_loc : Location.t
  ; pexp_loc_stack : location_stack
  ; pexp_attributes : attributes
  }

and expression_desc = Astlib.Ast_501.Parsetree.expression_desc =
  | Pexp_ident of Longident.t Asttypes.loc
  | Pexp_constant of constant
  | Pexp_let of Asttypes.rec_flag * value_binding list * expression
  | Pexp_function of case list
  | Pexp_fun of Asttypes.arg_label * expression option * pattern * expression
  | Pexp_apply of expression * (Asttypes.arg_label * expression) list
  | Pexp_match of expression * case list
  | Pexp_try of expression * case list
  | Pexp_tuple of expression list
  | Pexp_construct of Longident.t Asttypes.loc * expression option
  | Pexp_variant of string * expression option
  | Pexp_record of (Longident.t Asttypes.loc * expression) list * expression option
  | Pexp_field of expression * Longident.t Asttypes.loc
  | Pexp_setfield of expression * Longident.t Asttypes.loc * expression
  | Pexp_array of expression list
  | Pexp_ifthenelse of expression * expression * expression option
  | Pexp_sequence of expression * expression
  | Pexp_while of expression * expression
  | Pexp_for of pattern * expression * expression * Asttypes.direction_flag * expression
  | Pexp_constraint of expression * core_type
  | Pexp_coerce of expression * core_type option * core_type
  | Pexp_send of expression * string Asttypes.loc
  | Pexp_new of Longident.t Asttypes.loc
  | Pexp_setinstvar of string Asttypes.loc * expression
  | Pexp_override of (string Asttypes.loc * expression) list
  | Pexp_letmodule of string option Asttypes.loc * module_expr * expression
  | Pexp_letexception of extension_constructor * expression
  | Pexp_assert of expression
  | Pexp_lazy of expression
  | Pexp_poly of expression * core_type option
  | Pexp_object of class_structure
  | Pexp_newtype of string Asttypes.loc * expression
  | Pexp_pack of module_expr
  | Pexp_open of open_declaration * expression
  | Pexp_letop of letop
  | Pexp_extension of extension
  | Pexp_unreachable

and case = Astlib.Ast_501.Parsetree.case =
  { pc_lhs : pattern
  ; pc_guard : expression option
  ; pc_rhs : expression
  }

and letop = Astlib.Ast_501.Parsetree.letop =
  { let_ : binding_op
  ; ands : binding_op list
  ; body : expression
  }

and binding_op = Astlib.Ast_501.Parsetree.binding_op =
  { pbop_op : string Asttypes.loc
  ; pbop_pat : pattern
  ; pbop_exp : expression
  ; pbop_loc : Location.t
  }

and value_description = Astlib.Ast_501.Parsetree.value_description =
  { pval_name : string Asttypes.loc
  ; pval_type : core_type
  ; pval_prim : string list
  ; pval_attributes : attributes
  ; pval_loc : Location.t
  }

and type_declaration = Astlib.Ast_501.Parsetree.type_declaration =
  { ptype_name : string Asttypes.loc
  ; ptype_params : (core_type * (Asttypes.variance * Asttypes.injectivity)) list
  ; ptype_cstrs : (core_type * core_type * Location.t) list
  ; ptype_kind : type_kind
  ; ptype_private : Asttypes.private_flag
  ; ptype_manifest : core_type option
  ; ptype_attributes : attributes
  ; ptype_loc : Location.t
  }

and type_kind = Astlib.Ast_501.Parsetree.type_kind =
  | Ptype_abstract
  | Ptype_variant of constructor_declaration list
  | Ptype_record of label_declaration list
  | Ptype_open

and label_declaration = Astlib.Ast_501.Parsetree.label_declaration =
  { pld_name : string Asttypes.loc
  ; pld_mutable : Asttypes.mutable_flag
  ; pld_type : core_type
  ; pld_loc : Location.t
  ; pld_attributes : attributes
  }

and constructor_declaration = Astlib.Ast_501.Parsetree.constructor_declaration =
  { pcd_name : string Asttypes.loc
  ; pcd_vars : string Asttypes.loc list
  ; pcd_args : constructor_arguments
  ; pcd_res : core_type option
  ; pcd_loc : Location.t
  ; pcd_attributes : attributes
  }

and constructor_arguments = Astlib.Ast_501.Parsetree.constructor_arguments =
  | Pcstr_tuple of core_type list
  | Pcstr_record of label_declaration list

and type_extension = Astlib.Ast_501.Parsetree.type_extension =
  { ptyext_path : Longident.t Asttypes.loc
  ; ptyext_params : (core_type * (Asttypes.variance * Asttypes.injectivity)) list
  ; ptyext_constructors : extension_constructor list
  ; ptyext_private : Asttypes.private_flag
  ; ptyext_loc : Location.t
  ; ptyext_attributes : attributes
  }

and extension_constructor = Astlib.Ast_501.Parsetree.extension_constructor =
  { pext_name : string Asttypes.loc
  ; pext_kind : extension_constructor_kind
  ; pext_loc : Location.t
  ; pext_attributes : attributes
  }

and type_exception = Astlib.Ast_501.Parsetree.type_exception =
  { ptyexn_constructor : extension_constructor
  ; ptyexn_loc : Location.t
  ; ptyexn_attributes : attributes
  }

and extension_constructor_kind = Astlib.Ast_501.Parsetree.extension_constructor_kind =
  | Pext_decl of string Asttypes.loc list * constructor_arguments * core_type option
  | Pext_rebind of Longident.t Asttypes.loc

and class_type = Astlib.Ast_501.Parsetree.class_type =
  { pcty_desc : class_type_desc
  ; pcty_loc : Location.t
  ; pcty_attributes : attributes
  }

and class_type_desc = Astlib.Ast_501.Parsetree.class_type_desc =
  | Pcty_constr of Longident.t Asttypes.loc * core_type list
  | Pcty_signature of class_signature
  | Pcty_arrow of Asttypes.arg_label * core_type * class_type
  | Pcty_extension of extension
  | Pcty_open of open_description * class_type

and class_signature = Astlib.Ast_501.Parsetree.class_signature =
  { pcsig_self : core_type
  ; pcsig_fields : class_type_field list
  }

and class_type_field = Astlib.Ast_501.Parsetree.class_type_field =
  { pctf_desc : class_type_field_desc
  ; pctf_loc : Location.t
  ; pctf_attributes : attributes
  }

and class_type_field_desc = Astlib.Ast_501.Parsetree.class_type_field_desc =
  | Pctf_inherit of class_type
  | Pctf_val of
      (string Asttypes.loc * Asttypes.mutable_flag * Asttypes.virtual_flag * core_type)
  | Pctf_method of
      (string Asttypes.loc * Asttypes.private_flag * Asttypes.virtual_flag * core_type)
  | Pctf_constraint of (core_type * core_type)
  | Pctf_attribute of attribute
  | Pctf_extension of extension

and 'a class_infos = 'a Astlib.Ast_501.Parsetree.class_infos =
  { pci_virt : Asttypes.virtual_flag
  ; pci_params : (core_type * (Asttypes.variance * Asttypes.injectivity)) list
  ; pci_name : string Asttypes.loc
  ; pci_expr : 'a
  ; pci_loc : Location.t
  ; pci_attributes : attributes
  }

and class_description = class_type class_infos
and class_type_declaration = class_type class_infos

and class_expr = Astlib.Ast_501.Parsetree.class_expr =
  { pcl_desc : class_expr_desc
  ; pcl_loc : Location.t
  ; pcl_attributes : attributes
  }

and class_expr_desc = Astlib.Ast_501.Parsetree.class_expr_desc =
  | Pcl_constr of Longident.t Asttypes.loc * core_type list
  | Pcl_structure of class_structure
  | Pcl_fun of Asttypes.arg_label * expression option * pattern * class_expr
  | Pcl_apply of class_expr * (Asttypes.arg_label * expression) list
  | Pcl_let of Asttypes.rec_flag * value_binding list * class_expr
  | Pcl_constraint of class_expr * class_type
  | Pcl_extension of extension
  | Pcl_open of open_description * class_expr

and class_structure = Astlib.Ast_501.Parsetree.class_structure =
  { pcstr_self : pattern
  ; pcstr_fields : class_field list
  }

and class_field = Astlib.Ast_501.Parsetree.class_field =
  { pcf_desc : class_field_desc
  ; pcf_loc : Location.t
  ; pcf_attributes : attributes
  }

and class_field_desc = Astlib.Ast_501.Parsetree.class_field_desc =
  | Pcf_inherit of Asttypes.override_flag * class_expr * string Asttypes.loc option
  | Pcf_val of (string Asttypes.loc * Asttypes.mutable_flag * class_field_kind)
  | Pcf_method of (string Asttypes.loc * Asttypes.private_flag * class_field_kind)
  | Pcf_constraint of (core_type * core_type)
  | Pcf_initializer of expression
  | Pcf_attribute of attribute
  | Pcf_extension of extension

and class_field_kind = Astlib.Ast_501.Parsetree.class_field_kind =
  | Cfk_virtual of core_type
  | Cfk_concrete of Asttypes.override_flag * expression

and class_declaration = class_expr class_infos

and module_type = Astlib.Ast_501.Parsetree.module_type =
  { pmty_desc : module_type_desc
  ; pmty_loc : Location.t
  ; pmty_attributes : attributes
  }

and module_type_desc = Astlib.Ast_501.Parsetree.module_type_desc =
  | Pmty_ident of Longident.t Asttypes.loc
  | Pmty_signature of signature
  | Pmty_functor of functor_parameter * module_type
  | Pmty_with of module_type * with_constraint list
  | Pmty_typeof of module_expr
  | Pmty_extension of extension
  | Pmty_alias of Longident.t Asttypes.loc

and functor_parameter = Astlib.Ast_501.Parsetree.functor_parameter =
  | Unit
  | Named of string option Asttypes.loc * module_type

and signature = signature_item list

and signature_item = Astlib.Ast_501.Parsetree.signature_item =
  { psig_desc : signature_item_desc
  ; psig_loc : Location.t
  }

and signature_item_desc = Astlib.Ast_501.Parsetree.signature_item_desc =
  | Psig_value of value_description
  | Psig_type of Asttypes.rec_flag * type_declaration list
  | Psig_typesubst of type_declaration list
  | Psig_typext of type_extension
  | Psig_exception of type_exception
  | Psig_module of module_declaration
  | Psig_modsubst of module_substitution
  | Psig_recmodule of module_declaration list
  | Psig_modtype of module_type_declaration
  | Psig_modtypesubst of module_type_declaration
  | Psig_open of open_description
  | Psig_include of include_description
  | Psig_class of class_description list
  | Psig_class_type of class_type_declaration list
  | Psig_attribute of attribute
  | Psig_extension of extension * attributes

and module_declaration = Astlib.Ast_501.Parsetree.module_declaration =
  { pmd_name : string option Asttypes.loc
  ; pmd_type : module_type
  ; pmd_attributes : attributes
  ; pmd_loc : Location.t
  }

and module_substitution = Astlib.Ast_501.Parsetree.module_substitution =
  { pms_name : string Asttypes.loc
  ; pms_manifest : Longident.t Asttypes.loc
  ; pms_attributes : attributes
  ; pms_loc : Location.t
  }

and module_type_declaration = Astlib.Ast_501.Parsetree.module_type_declaration =
  { pmtd_name : string Asttypes.loc
  ; pmtd_type : module_type option
  ; pmtd_attributes : attributes
  ; pmtd_loc : Location.t
  }

and 'a open_infos = 'a Astlib.Ast_501.Parsetree.open_infos =
  { popen_expr : 'a
  ; popen_override : Asttypes.override_flag
  ; popen_loc : Location.t
  ; popen_attributes : attributes
  }

and open_description = Longident.t Asttypes.loc open_infos
and open_declaration = module_expr open_infos

and 'a include_infos = 'a Astlib.Ast_501.Parsetree.include_infos =
  { pincl_mod : 'a
  ; pincl_loc : Location.t
  ; pincl_attributes : attributes
  }

and include_description = module_type include_infos
and include_declaration = module_expr include_infos

and with_constraint = Astlib.Ast_501.Parsetree.with_constraint =
  | Pwith_type of Longident.t Asttypes.loc * type_declaration
  | Pwith_module of Longident.t Asttypes.loc * Longident.t Asttypes.loc
  | Pwith_modtype of Longident.t Asttypes.loc * module_type
  | Pwith_modtypesubst of Longident.t Asttypes.loc * module_type
  | Pwith_typesubst of Longident.t Asttypes.loc * type_declaration
  | Pwith_modsubst of Longident.t Asttypes.loc * Longident.t Asttypes.loc

and module_expr = Astlib.Ast_501.Parsetree.module_expr =
  { pmod_desc : module_expr_desc
  ; pmod_loc : Location.t
  ; pmod_attributes : attributes
  }

and module_expr_desc = Astlib.Ast_501.Parsetree.module_expr_desc =
  | Pmod_ident of Longident.t Asttypes.loc
  | Pmod_structure of structure
  | Pmod_functor of functor_parameter * module_expr
  | Pmod_apply of module_expr * module_expr
  | Pmod_apply_unit of module_expr
  | Pmod_constraint of module_expr * module_type
  | Pmod_unpack of expression
  | Pmod_extension of extension

and structure = structure_item list

and structure_item = Astlib.Ast_501.Parsetree.structure_item =
  { pstr_desc : structure_item_desc
  ; pstr_loc : Location.t
  }

and structure_item_desc = Astlib.Ast_501.Parsetree.structure_item_desc =
  | Pstr_eval of expression * attributes
  | Pstr_value of Asttypes.rec_flag * value_binding list
  | Pstr_primitive of value_description
  | Pstr_type of Asttypes.rec_flag * type_declaration list
  | Pstr_typext of type_extension
  | Pstr_exception of type_exception
  | Pstr_module of module_binding
  | Pstr_recmodule of module_binding list
  | Pstr_modtype of module_type_declaration
  | Pstr_open of open_declaration
  | Pstr_class of class_declaration list
  | Pstr_class_type of class_type_declaration list
  | Pstr_include of include_declaration
  | Pstr_attribute of attribute
  | Pstr_extension of extension * attributes

and value_constraint = Astlib.Ast_501.Parsetree.value_constraint =
  | Pvc_constraint of
      { locally_abstract_univars : string Asttypes.loc list
      ; typ : core_type
      }
  | Pvc_coercion of
      { ground : core_type option
      ; coercion : core_type
      }

and value_binding = Astlib.Ast_501.Parsetree.value_binding =
  { pvb_pat : pattern
  ; pvb_expr : expression
  ; pvb_constraint : value_constraint option
  ; pvb_attributes : attributes
  ; pvb_loc : Location.t
  }

and module_binding = Astlib.Ast_501.Parsetree.module_binding =
  { pmb_name : string option Asttypes.loc
  ; pmb_expr : module_expr
  ; pmb_attributes : attributes
  ; pmb_loc : Location.t
  }
[@@deriving sexp_of]

type toplevel_phrase = Astlib.Ast_501.Parsetree.toplevel_phrase =
  | Ptop_def of structure
  | Ptop_dir of toplevel_directive

and toplevel_directive = Astlib.Ast_501.Parsetree.toplevel_directive =
  { pdir_name : string Asttypes.loc
  ; pdir_arg : directive_argument option
  ; pdir_loc : Location.t
  }
[@@deriving sexp_of]

and directive_argument = Astlib.Ast_501.Parsetree.directive_argument =
  { pdira_desc : directive_argument_desc
  ; pdira_loc : Location.t
  }

and directive_argument_desc = Astlib.Ast_501.Parsetree.directive_argument_desc =
  | Pdir_string of string
  | Pdir_int of string * char option
  | Pdir_ident of Longident.t
  | Pdir_bool of bool
[@@deriving sexp_of]
