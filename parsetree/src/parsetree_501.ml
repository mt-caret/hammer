open! Core
module Parsetree = Astlib.Ast_501.Parsetree

module Astlib__ = struct
  module Location = struct
    type t = Astlib.Location.t =
      { loc_start : Source_code_position.t
      ; loc_end : Source_code_position.t
      ; loc_ghost : bool
      }
    [@@deriving sexp_of]

    type%import 'a loc = 'a Astlib.Location.loc
  end

  module Longident = struct
    type%import t = Astlib.Longident.t [@@deriving sexp_of]
  end
end

module Asttypes = struct
  type%import constant = Astlib.Ast_501.Asttypes.constant [@@deriving sexp_of]
  type%import rec_flag = Astlib.Ast_501.Asttypes.rec_flag [@@deriving sexp_of]
  type%import direction_flag = Astlib.Ast_501.Asttypes.direction_flag [@@deriving sexp_of]
  type%import private_flag = Astlib.Ast_501.Asttypes.private_flag [@@deriving sexp_of]
  type%import mutable_flag = Astlib.Ast_501.Asttypes.mutable_flag [@@deriving sexp_of]
  type%import virtual_flag = Astlib.Ast_501.Asttypes.virtual_flag [@@deriving sexp_of]
  type%import override_flag = Astlib.Ast_501.Asttypes.override_flag [@@deriving sexp_of]
  type%import closed_flag = Astlib.Ast_501.Asttypes.closed_flag [@@deriving sexp_of]
  type%import label = Astlib.Ast_501.Asttypes.label [@@deriving sexp_of]
  type%import arg_label = Astlib.Ast_501.Asttypes.arg_label [@@deriving sexp_of]
  type%import 'a loc = 'a Astlib.Ast_501.Asttypes.loc [@@deriving sexp_of]
  type%import variance = Astlib.Ast_501.Asttypes.variance [@@deriving sexp_of]
  type%import injectivity = Astlib.Ast_501.Asttypes.injectivity [@@deriving sexp_of]
end

type%import constant = Astlib.Ast_501.Parsetree.constant [@@deriving sexp_of]
type%import location_stack = Astlib.Ast_501.Parsetree.location_stack [@@deriving sexp_of]

type%import attribute = Astlib.Ast_501.Parsetree.attribute
and extension = Astlib.Ast_501.Parsetree.extension
and attributes = Astlib.Ast_501.Parsetree.attributes
and payload = Astlib.Ast_501.Parsetree.payload
and core_type = Astlib.Ast_501.Parsetree.core_type
and core_type_desc = Astlib.Ast_501.Parsetree.core_type_desc
and package_type = Astlib.Ast_501.Parsetree.package_type
and row_field = Astlib.Ast_501.Parsetree.row_field
and row_field_desc = Astlib.Ast_501.Parsetree.row_field_desc
and object_field = Astlib.Ast_501.Parsetree.object_field
and object_field_desc = Astlib.Ast_501.Parsetree.object_field_desc
and pattern = Astlib.Ast_501.Parsetree.pattern
and pattern_desc = Astlib.Ast_501.Parsetree.pattern_desc
and expression = Astlib.Ast_501.Parsetree.expression
and expression_desc = Astlib.Ast_501.Parsetree.expression_desc
and case = Astlib.Ast_501.Parsetree.case
and letop = Astlib.Ast_501.Parsetree.letop
and binding_op = Astlib.Ast_501.Parsetree.binding_op
and value_description = Astlib.Ast_501.Parsetree.value_description
and type_declaration = Astlib.Ast_501.Parsetree.type_declaration
and type_kind = Astlib.Ast_501.Parsetree.type_kind
and label_declaration = Astlib.Ast_501.Parsetree.label_declaration
and constructor_declaration = Astlib.Ast_501.Parsetree.constructor_declaration
and constructor_arguments = Astlib.Ast_501.Parsetree.constructor_arguments
and type_extension = Astlib.Ast_501.Parsetree.type_extension
and extension_constructor = Astlib.Ast_501.Parsetree.extension_constructor
and type_exception = Astlib.Ast_501.Parsetree.type_exception
and extension_constructor_kind = Astlib.Ast_501.Parsetree.extension_constructor_kind
and class_type = Astlib.Ast_501.Parsetree.class_type
and class_type_desc = Astlib.Ast_501.Parsetree.class_type_desc
and class_signature = Astlib.Ast_501.Parsetree.class_signature
and class_type_field = Astlib.Ast_501.Parsetree.class_type_field
and class_type_field_desc = Astlib.Ast_501.Parsetree.class_type_field_desc
and 'a class_infos = 'a Astlib.Ast_501.Parsetree.class_infos
and class_description = Astlib.Ast_501.Parsetree.class_description
and class_type_declaration = Astlib.Ast_501.Parsetree.class_type_declaration
and class_expr = Astlib.Ast_501.Parsetree.class_expr
and class_expr_desc = Astlib.Ast_501.Parsetree.class_expr_desc
and class_structure = Astlib.Ast_501.Parsetree.class_structure
and class_field = Astlib.Ast_501.Parsetree.class_field
and class_field_desc = Astlib.Ast_501.Parsetree.class_field_desc
and class_field_kind = Astlib.Ast_501.Parsetree.class_field_kind
and class_declaration = Astlib.Ast_501.Parsetree.class_declaration
and module_type = Astlib.Ast_501.Parsetree.module_type
and module_type_desc = Astlib.Ast_501.Parsetree.module_type_desc
and functor_parameter = Astlib.Ast_501.Parsetree.functor_parameter
and signature = Astlib.Ast_501.Parsetree.signature
and signature_item = Astlib.Ast_501.Parsetree.signature_item
and signature_item_desc = Astlib.Ast_501.Parsetree.signature_item_desc
and module_declaration = Astlib.Ast_501.Parsetree.module_declaration
and module_substitution = Astlib.Ast_501.Parsetree.module_substitution
and module_type_declaration = Astlib.Ast_501.Parsetree.module_type_declaration
and 'a open_infos = 'a Astlib.Ast_501.Parsetree.open_infos
and open_description = Astlib.Ast_501.Parsetree.open_description
and open_declaration = Astlib.Ast_501.Parsetree.open_declaration
and 'a include_infos = 'a Astlib.Ast_501.Parsetree.include_infos
and include_description = Astlib.Ast_501.Parsetree.include_description
and include_declaration = Astlib.Ast_501.Parsetree.include_declaration
and with_constraint = Astlib.Ast_501.Parsetree.with_constraint
and module_expr = Astlib.Ast_501.Parsetree.module_expr
and module_expr_desc = Astlib.Ast_501.Parsetree.module_expr_desc
and structure = Astlib.Ast_501.Parsetree.structure
and structure_item = Astlib.Ast_501.Parsetree.structure_item
and structure_item_desc = Astlib.Ast_501.Parsetree.structure_item_desc
and value_constraint = Astlib.Ast_501.Parsetree.value_constraint
and value_binding = Astlib.Ast_501.Parsetree.value_binding
and module_binding = Astlib.Ast_501.Parsetree.module_binding [@@deriving sexp_of]

type%import toplevel_phrase = Astlib.Ast_501.Parsetree.toplevel_phrase
and toplevel_directive = Astlib.Ast_501.Parsetree.toplevel_directive
and directive_argument = Astlib.Ast_501.Parsetree.directive_argument

and directive_argument_desc = Astlib.Ast_501.Parsetree.directive_argument_desc
[@@deriving sexp_of]
