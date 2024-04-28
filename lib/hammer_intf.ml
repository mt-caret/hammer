open! Core

module type S = sig
  type t [@@deriving sexp_of]

  val sampler : t Sampler.t
end

module type S1 = sig
  type 'a t [@@deriving sexp_of]

  val sampler : 'a Sampler.t -> 'a t Sampler.t
end

module type S2 = sig
  type ('a, 'b) t [@@deriving sexp_of]

  val sampler : 'a Sampler.t -> 'b Sampler.t -> ('a, 'b) t Sampler.t
end

module type Hammer = sig
  module State = State
  module Sampler = Sampler

  module type S = S
  module type S1 = S1
  module type S2 = S2

  module Test : sig
    type t

    val create : (module S with type t = 'a) -> f:('a -> unit) -> t
  end

  val create_command : Test.t list -> Command.t
end
