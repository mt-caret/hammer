open! Core

module State : sig
  type t

  val create_random : seed:int array -> t
  val create_in_channel : in_channel:In_channel.t -> t
  val int : t -> int
  val non_negative_int : t -> int
  val char : t -> char
  val byte : t -> int
  val bool : t -> bool
  val choose : t -> 'a array -> 'a
end

module type S = sig
  type t [@@deriving sexp_of]

  val sample : State.t -> t
end

module Test : sig
  type t

  val create : (module S with type t = 'a) -> f:('a -> unit) -> t
end

val create_command : Test.t list -> Command.t
