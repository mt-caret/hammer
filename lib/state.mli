open! Core

type t

val create_random : seed:int array -> t
val create_in_channel : in_channel:In_channel.t -> t
val int : t -> int
val non_negative_int : t -> int
val int_incl : t -> int -> int -> int
val int32 : t -> int32
val int64 : t -> int64
val nativeint : t -> nativeint
val char : t -> char
val byte : t -> int
val bool : t -> bool
val string : t -> size:int -> string
val choose : t -> 'a array -> 'a
