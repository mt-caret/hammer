open! Core

type +'a t

val create : (State.t -> 'a) -> 'a t
val sample : 'a t -> State.t -> 'a

include Monad.S with type 'a t := 'a t

val size : int t
val sampler_int : int t
val sampler_int32 : int32 t
val sampler_int64 : int64 t
val sampler_nativeint : nativeint t
val sampler_char : char t
val sampler_bool : bool t
val sampler_string : string t
val sampler_option : 'a t -> 'a option t
val sampler_list : 'a t -> 'a list t
val fixed_point : ('a t -> 'a t) -> 'a t
val choose : 'a list -> 'a t
val choose_samplers : 'a t list -> 'a t
