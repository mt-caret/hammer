open! Base
open! Ppxlib
open! Import

type t

val create : type_declaration -> t
val type_variable_samplers : t -> (label, expression, String.comparator_witness) Map.t
val type_variable_patterns : t -> pattern list
val core_type_for_annotation : t -> wrapper:(core_type -> core_type) -> core_type
