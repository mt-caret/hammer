open! Base
open! Ppxlib

type t

val create : type_declaration -> t loc
val to_sampler_expression : t loc -> expression
