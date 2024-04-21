open! Base
open! Ppxlib

val sig_type_decl : (signature, rec_flag * type_declaration list) Deriving.Generator.t
val str_type_decl : (structure, rec_flag * type_declaration list) Deriving.Generator.t
