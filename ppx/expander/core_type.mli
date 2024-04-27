open! Base
open! Ppxlib

type t

val create
  :  type_declaration
  -> recursive_samplers:(string, expression, _) Map.t option
  -> t loc

val to_sampler_expression : t loc -> expression
