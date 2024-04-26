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
  let (sampler : t Hammer.Sampler.t) = sampler_int
  let _ = sampler

  [@@@end]
end

module Simple_module_type = struct
  type t = Simple_int.t [@@deriving sexp_of] [@@deriving_inline hammer]

  let _ = fun (_ : t) -> ()
  let (sampler : t Hammer.Sampler.t) = Simple_int.sampler
  let _ = sampler

  [@@@end]
end

module Simple_int_with_attribute = struct
  type t = (int[@hammer.sampler Hammer.Sampler.create Hammer.State.int])
  [@@deriving sexp_of] [@@deriving_inline hammer]

  let _ = fun (_ : t) -> ()
  let (sampler : t Hammer.Sampler.t) = Hammer.Sampler.create Hammer.State.int
  let _ = sampler

  [@@@end]
end

module Tuple = struct
  open! Hammer.Sampler

  type t = int * bool [@@deriving sexp_of] [@@deriving_inline hammer]

  let _ = fun (_ : t) -> ()

  let (sampler : t Hammer.Sampler.t) =
    Hammer.Sampler.create (fun _state__005_ ->
      ( Hammer.Sampler.sample sampler_int _state__005_
      , Hammer.Sampler.sample sampler_bool _state__005_ ))
  ;;

  let _ = sampler

  [@@@end]
end

module Record = struct
  open! Hammer.Sampler

  type t =
    { a : int
    ; b : bool
    }
  [@@deriving sexp_of] [@@deriving_inline hammer]

  let _ = fun (_ : t) -> ()

  let (sampler : t Hammer.Sampler.t) =
    Hammer.Sampler.create (fun _state__011_ ->
      { a = Hammer.Sampler.sample sampler_int _state__011_
      ; b = Hammer.Sampler.sample sampler_bool _state__011_
      })
  ;;

  let _ = sampler

  [@@@end]
end

module Variant = struct
  open! Hammer.Sampler

  type t =
    | A
    | B of int
    | C of int * bool
    | D of
        { a : int
        ; b : bool
        }
  [@@deriving sexp_of] [@@deriving_inline hammer]

  let _ = fun (_ : t) -> ()

  let (sampler : t Hammer.Sampler.t) =
    Hammer.Sampler.choose_samplers
      [ Hammer.Sampler.return A
      ; Hammer.Sampler.create (fun _state__023_ ->
          B (Hammer.Sampler.sample sampler_int _state__023_))
      ; Hammer.Sampler.create (fun _state__024_ ->
          C
            ( Hammer.Sampler.sample sampler_int _state__024_
            , Hammer.Sampler.sample sampler_bool _state__024_ ))
      ; Hammer.Sampler.create (fun _state__025_ ->
          D
            { a = Hammer.Sampler.sample sampler_int _state__025_
            ; b = Hammer.Sampler.sample sampler_bool _state__025_
            })
      ]
  ;;

  let _ = sampler

  [@@@end]
end

module Simple_polymorphic_variant = struct
  open! Hammer.Sampler

  type t =
    [ `A
    | `B of int
    | `C of int * bool
    ]
  [@@deriving sexp_of] [@@deriving_inline hammer]

  let _ = fun (_ : t) -> ()

  let (sampler : t Hammer.Sampler.t) =
    Hammer.Sampler.choose_samplers
      [ Hammer.Sampler.return `A
      ; Hammer.Sampler.create (fun _state__032_ ->
          `B (Hammer.Sampler.sample sampler_int _state__032_))
      ; Hammer.Sampler.create (fun _state__033_ ->
          `C
            (Hammer.Sampler.sample
               (Hammer.Sampler.create (fun _state__034_ ->
                  ( Hammer.Sampler.sample sampler_int _state__034_
                  , Hammer.Sampler.sample sampler_bool _state__034_ )))
               _state__033_))
      ]
  ;;

  let _ = sampler

  [@@@end]
end

module Polymorphic_variant_inheritance = struct
  type t =
    [ Simple_polymorphic_variant.t
    | `D
    ]
  [@@deriving sexp_of] [@@deriving_inline hammer]

  let _ = fun (_ : t) -> ()

  let (sampler : t Hammer.Sampler.t) =
    Hammer.Sampler.choose_samplers
      [ (Simple_polymorphic_variant.sampler
          :> [ Simple_polymorphic_variant.t | `D ] Hammer.Sampler.t)
      ; Hammer.Sampler.return `D
      ]
  ;;

  let _ = sampler

  [@@@end]
end
