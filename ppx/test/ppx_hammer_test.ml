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
  (* TODO: remove requirement to open [Hammer.Sampler] in type definitions *)
  open Hammer.Sampler

  type t = int [@@deriving sexp_of] [@@deriving_inline hammer]

  let _ = fun (_ : t) -> ()
  let sampler = sampler_int
  let _ = sampler

  [@@@end]
end

module Simple_module_type = struct
  type t = Simple_int.t [@@deriving sexp_of] [@@deriving_inline hammer]

  let _ = fun (_ : t) -> ()
  let sampler = Simple_int.sampler
  let _ = sampler

  [@@@end]
end

module Simple_int_with_attribute = struct
  type t = (int[@hammer.sampler Hammer.Sampler.create Hammer.State.int])
  [@@deriving sexp_of] [@@deriving_inline hammer]

  let _ = fun (_ : t) -> ()
  let sampler = Hammer.Sampler.create Hammer.State.int
  let _ = sampler

  [@@@end]
end

(* TODO: removing the signature results in type generalization errors;
   ppx_hammer should generate type annotations. *)
module Simple_polymorphic_variant : sig
  type t =
    [ `A
    | `B of int
    ]
  [@@deriving hammer]
end = struct
  open! Hammer.Sampler

  type t =
    [ `A
    | `B of int
    ]
  [@@deriving sexp_of] [@@deriving_inline hammer]

  let _ = fun (_ : t) -> ()

  let sampler =
    Hammer.Sampler.choose_samplers
      [ Hammer.Sampler.return `A
      ; Hammer.Sampler.map sampler_int ~f:(fun _sample__002_ -> `B _sample__002_)
      ]
  ;;

  let _ = sampler

  [@@@end]
end

module Polymorphic_variant_inheritance : sig
  type t =
    [ Simple_polymorphic_variant.t
    | `C
    ]
  [@@deriving hammer]
end = struct
  type t =
    [ Simple_polymorphic_variant.t
    | `C
    ]
  [@@deriving sexp_of] [@@deriving_inline hammer]

  let _ = fun (_ : t) -> ()

  let sampler =
    Hammer.Sampler.choose_samplers
      [ (Simple_polymorphic_variant.sampler
          :> [ Simple_polymorphic_variant.t | `C ] Hammer.Sampler.t)
      ; Hammer.Sampler.return `C
      ]
  ;;

  let _ = sampler

  [@@@end]
end
