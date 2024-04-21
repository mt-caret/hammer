open! Core

module Simple_int : sig
  type t [@@deriving_inline hammer]

  include sig
    [@@@ocaml.warning "-32"]

    include Hammer.S with type t := t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end = struct
  type t = int [@@deriving sexp_of]

  let sampler =
    let open Hammer in
    Sampler.create State.int
  ;;
end

module Simple_int_with_attribute : sig
  type t [@@deriving hammer]
end = struct
  type t = (int[@hammer.sampler Hammer.Sampler.create Hammer.State.int])
  [@@deriving sexp_of] [@@deriving_inline hammer]

  let _ = fun (_ : t) -> ()
  let sampler = Hammer.Sampler.create Hammer.State.int
  let _ = sampler

  [@@@end]
end

module Simple_int2 : sig
  type t [@@deriving hammer]
end = struct
  (* TODO: remove requirement to open [Hammer.Sampler] in type definitions *)
  open Hammer.Sampler

  type t = int [@@deriving sexp_of] [@@deriving_inline hammer]

  let _ = fun (_ : t) -> ()
  let sampler = sampler_int
  let _ = sampler

  [@@@end]
end
