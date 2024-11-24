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

module Simple_module_type : Hammer.S = struct
  type t = Simple_int.t [@@deriving sexp_of] [@@deriving_inline hammer]

  let _ = fun (_ : t) -> ()
  let (sampler : t Hammer.Sampler.t) = Simple_int.sampler
  let _ = sampler

  [@@@end]
end

module Simple_int_with_attribute : Hammer.S = struct
  type t = (int[@hammer.sampler Hammer.Sampler.create Hammer.State.int])
  [@@deriving sexp_of] [@@deriving_inline hammer]

  let _ = fun (_ : t) -> ()
  let (sampler : t Hammer.Sampler.t) = Hammer.Sampler.create Hammer.State.int
  let _ = sampler

  [@@@end]
end

module Tuple : Hammer.S = struct
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

module Record : Hammer.S = struct
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

module Variant : Hammer.S = struct
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

module Simple_polymorphic_variant : sig
  type t =
    [ `A
    | `B of int
    | `C of int * bool
    ]
  [@@deriving sexp_of, hammer]
end = struct
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

module Polymorphic_variant_inheritance : Hammer.S = struct
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

module Simple_recursive_type : Hammer.S = struct
  type t =
    | Leaf
    | Node of t * t
  [@@deriving sexp_of] [@@deriving_inline hammer]

  let _ = fun (_ : t) -> ()

  let rec (sampler : t Hammer.Sampler.t lazy_t) =
    lazy
      (Hammer.Sampler.choose_samplers
         [ Hammer.Sampler.return Leaf
         ; Hammer.Sampler.create (fun _state__040_ ->
             Node
               ( Hammer.Sampler.sample (force sampler) _state__040_
               , Hammer.Sampler.sample (force sampler) _state__040_ ))
         ])
  ;;

  let _ = sampler
  let sampler = force sampler
  let _ = sampler

  [@@@end]

  let%expect_test "recursive sampler" =
    let state = Hammer.State.create_random ~seed:[||] in
    for _ = 1 to 5 do
      Hammer.Sampler.sample sampler state |> [%sexp_of: t] |> print_s
    done;
    [%expect
      {|
      Leaf
      (Node Leaf Leaf)
      Leaf
      (Node
       (Node (Node Leaf (Node (Node Leaf Leaf) (Node Leaf (Node Leaf Leaf))))
        (Node Leaf Leaf))
       Leaf)
      Leaf |}]
  ;;
end

module Mutually_recursive_types : sig
  type t [@@deriving sexp_of, hammer]

  (* BUG: nothing is derived here!? *)
  type s [@@deriving sexp_of] [@@deriving_inline hammer]

  [@@@end]
end = struct
  type t =
    | Leaf
    | Node of s * s

  and s =
    | Empty
    | Non_empty of t
  [@@deriving sexp_of] [@@deriving_inline hammer]

  let _ = fun (_ : t) -> ()
  let _ = fun (_ : s) -> ()

  let rec (sampler : t Hammer.Sampler.t lazy_t) =
    lazy
      (Hammer.Sampler.choose_samplers
         [ Hammer.Sampler.return Leaf
         ; Hammer.Sampler.create (fun _state__047_ ->
             Node
               ( Hammer.Sampler.sample (force sampler_s) _state__047_
               , Hammer.Sampler.sample (force sampler_s) _state__047_ ))
         ])

  and (sampler_s : s Hammer.Sampler.t lazy_t) =
    lazy
      (Hammer.Sampler.choose_samplers
         [ Hammer.Sampler.return Empty
         ; Hammer.Sampler.create (fun _state__048_ ->
             Non_empty (Hammer.Sampler.sample (force sampler) _state__048_))
         ])
  ;;

  let _ = sampler
  and _ = sampler_s

  let sampler = force sampler
  and sampler_s = force sampler_s

  let _ = sampler
  and _ = sampler_s

  [@@@end]

  let%expect_test "recursive sampler" =
    let state = Hammer.State.create_random ~seed:[||] in
    for _ = 1 to 5 do
      Hammer.Sampler.sample sampler state |> [%sexp_of: t] |> print_s
    done;
    [%expect
      {|
           Leaf
           (Node Empty Empty)
           Leaf
           (Node (Non_empty (Node Empty Empty)) Empty)
           (Node Empty (Non_empty (Node Empty (Non_empty Leaf)))) |}]
  ;;
end

module Simple_type_with_parameters : sig
  type ('a, 'b) t [@@deriving sexp_of] [@@deriving_inline hammer]

  include sig
    [@@@ocaml.warning "-32"]

    include Hammer.S2 with type ('a, 'b) t := ('a, 'b) t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]
end = struct
  open! Hammer.Sampler

  type ('a, 'b) t = 'a * 'b * int [@@deriving sexp_of] [@@deriving_inline hammer]

  let _ = fun (_ : ('a, 'b) t) -> ()

  let sampler
    : 'a 'b. 'a Hammer.Sampler.t -> 'b Hammer.Sampler.t -> ('a, 'b) t Hammer.Sampler.t
    =
    fun sampler_a sampler_b ->
    Hammer.Sampler.create (fun _state__057_ ->
      ( Hammer.Sampler.sample sampler_a _state__057_
      , Hammer.Sampler.sample sampler_b _state__057_
      , Hammer.Sampler.sample sampler_int _state__057_ ))
  ;;

  let _ = sampler

  [@@@end]
end

module Recursive_parameterized_type : sig
  [@@@ocaml.warning "-32"]

  type 'a t

  (* BUG: OCaml is inferring a weak type variable, which is not what we want *)
  val sampler : bool Hammer.Sampler.t -> bool t Hammer.Sampler.t
end = struct
  open! Hammer.Sampler

  type 'a t = 'a list =
    | []
    | ( :: ) of 'a * 'a t
  [@@deriving sexp_of] [@@deriving_inline hammer]

  let _ = fun (_ : 'a t) -> ()

  let rec sampler : 'a. ('a Hammer.Sampler.t -> 'a t Hammer.Sampler.t) lazy_t =
    lazy
      (fun sampler_a ->
        Hammer.Sampler.choose_samplers
          [ Hammer.Sampler.return []
          ; Hammer.Sampler.create (fun _state__064_ ->
              Hammer.Sampler.sample sampler_a _state__064_
              :: Hammer.Sampler.sample ((force sampler) sampler_a) _state__064_)
          ])
  ;;

  let _ = sampler
  let sampler = force sampler
  let _ = sampler

  [@@@end]

  let%expect_test "recursive sampler" =
    let state = Hammer.State.create_random ~seed:[||] in
    for _ = 1 to 5 do
      Hammer.Sampler.sample (sampler sampler_bool) state
      |> [%sexp_of: bool list]
      |> print_s
    done;
    [%expect {|
      ()
      (false)
      ()
      (true)
      (false) |}]
  ;;
end

module Mutually_recursive_parametrized_types : sig
  [@@@ocaml.warning "-32"]
  [@@@ocaml.warning "-34"]

  type 'a t
  type 'a s

  (* BUG: OCaml is inferring a weak type variable, which is not what we want *)
  val sampler : bool Hammer.Sampler.t -> bool t Hammer.Sampler.t
  val sampler_s : bool Hammer.Sampler.t -> bool s Hammer.Sampler.t
end = struct
  open! Hammer.Sampler

  type 'a t =
    | Leaf
    | Node of 'a s * 'a s

  and 'a s =
    | Just of 'a
    | Weird of int t
    | Non_empty of 'a t
  [@@deriving sexp_of] [@@deriving_inline hammer]

  let _ = fun (_ : 'a t) -> ()
  let _ = fun (_ : 'a s) -> ()

  let rec sampler : 'a. ('a Hammer.Sampler.t -> 'a t Hammer.Sampler.t) lazy_t =
    lazy
      (fun sampler_a ->
        Hammer.Sampler.choose_samplers
          [ Hammer.Sampler.return Leaf
          ; Hammer.Sampler.create (fun _state__080_ ->
              Node
                ( Hammer.Sampler.sample ((force sampler_s) sampler_a) _state__080_
                , Hammer.Sampler.sample ((force sampler_s) sampler_a) _state__080_ ))
          ])

  and sampler_s : 'a. ('a Hammer.Sampler.t -> 'a s Hammer.Sampler.t) lazy_t =
    lazy
      (fun sampler_a ->
        Hammer.Sampler.choose_samplers
          [ Hammer.Sampler.create (fun _state__081_ ->
              Just (Hammer.Sampler.sample sampler_a _state__081_))
          ; Hammer.Sampler.create (fun _state__082_ ->
              Weird (Hammer.Sampler.sample ((force sampler) sampler_int) _state__082_))
          ; Hammer.Sampler.create (fun _state__083_ ->
              Non_empty (Hammer.Sampler.sample ((force sampler) sampler_a) _state__083_))
          ])
  ;;

  let _ = sampler
  and _ = sampler_s

  let sampler = force sampler
  and sampler_s = force sampler_s

  let _ = sampler
  and _ = sampler_s

  [@@@end]

  let%expect_test "recursive sampler" =
    let state = Hammer.State.create_random ~seed:[||] in
    for _ = 1 to 5 do
      Hammer.Sampler.sample (sampler sampler_bool) state |> [%sexp_of: bool t] |> print_s
    done;
    [%expect
      {|
      Leaf
      (Node
       (Non_empty
        (Node (Weird (Node (Non_empty Leaf) (Non_empty Leaf)))
         (Non_empty (Node (Just false) (Weird Leaf)))))
       (Weird Leaf))
      (Node (Non_empty Leaf) (Non_empty Leaf))
      (Node (Non_empty Leaf) (Just false))
      (Node (Just true) (Non_empty (Node (Just true) (Weird Leaf)))) |}]
  ;;
end
