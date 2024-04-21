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
      let _sample0__006_ = Hammer.Sampler.sample sampler_int _state__005_
      and _sample1__007_ = Hammer.Sampler.sample sampler_bool _state__005_ in
      _sample0__006_, _sample1__007_)
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
    Hammer.Sampler.create (fun _state__013_ ->
      let a = Hammer.Sampler.sample sampler_int _state__013_
      and b = Hammer.Sampler.sample sampler_bool _state__013_ in
      { a; b })
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
      ; Hammer.Sampler.map sampler_int ~f:(fun _sample__020_ -> `B _sample__020_)
      ; Hammer.Sampler.map
          (Hammer.Sampler.create (fun _state__022_ ->
             let _sample0__023_ = Hammer.Sampler.sample sampler_int _state__022_
             and _sample1__024_ = Hammer.Sampler.sample sampler_bool _state__022_ in
             _sample0__023_, _sample1__024_))
          ~f:(fun _sample__021_ -> `C _sample__021_)
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
