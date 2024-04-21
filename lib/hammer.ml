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
end = struct
  type t =
    | Random of Random.State.t
    | In_channel of In_channel.t

  let create_random ~seed = Random (Random.State.make seed)
  let create_in_channel ~in_channel = In_channel in_channel

  exception Eof

  let int = function
    | Random state -> Random.State.int_incl state Int.min_value Int.max_value
    | In_channel in_channel ->
      (match In_channel.input_binary_int in_channel with
       | None -> raise Eof
       | Some n -> n)
  ;;

  let non_negative_int = function
    | Random state -> Random.State.int_incl state 0 Int.max_value
    | In_channel in_channel ->
      (match In_channel.input_binary_int in_channel with
       | None -> raise Eof
       | Some n -> if Int.is_non_negative n then n else -n)
  ;;

  let char = function
    | Random state -> Random.State.char state
    | In_channel in_channel ->
      (match In_channel.input_char in_channel with
       | None -> raise Eof
       | Some ch -> ch)
  ;;

  let byte t = char t |> Char.to_int

  let bool = function
    | Random state -> Random.State.bool state
    | In_channel in_channel ->
      (match In_channel.input_char in_channel with
       | None -> raise Eof
       | Some ch -> Char.to_int ch land 1 = 1)
  ;;

  let choose t elements =
    [%test_pred: _ array] (Fn.non Array.is_empty) elements;
    match elements with
    | [| element |] -> element
    | _ ->
      let num_elements = Array.length elements in
      if num_elements < 256
      then Array.unsafe_get elements (byte t % num_elements)
      else Array.unsafe_get elements (non_negative_int t % num_elements)
  ;;
end

module Sampler : sig
  type +'a t

  val create : (State.t -> 'a) -> 'a t
  val sample : 'a t -> State.t -> 'a

  include Monad.S with type 'a t := 'a t

  val sampler_int : int t
  val fixed_point : ('a t -> 'a t) -> 'a t
  val choose : 'a list -> 'a t
  val choose_samplers : 'a t list -> 'a t
end = struct
  module T = struct
    type 'a t = { f : State.t -> 'a } [@@unboxed]

    let create f = { f }
    let sample t state = t.f state
    let return x = { f = Fn.const x }
    let map t ~f = { f = Fn.compose f t.f }

    let bind t ~f =
      { f =
          (fun state ->
            let x = sample t state in
            sample (f x) state)
      }
    ;;

    let sampler_int = create State.int

    let fixed_point apply =
      let rec lazy_t = lazy (apply { f = (fun state -> sample (force lazy_t) state) }) in
      force lazy_t
    ;;

    let choose elements =
      let elements = Array.of_list elements in
      create (fun state -> State.choose state elements)
    ;;

    let choose_samplers ts =
      let t = choose ts in
      create (fun state ->
        let t' = sample t state in
        sample t' state)
    ;;
  end

  include T

  include Monad.Make (struct
      include T

      let map = `Custom map
    end)
end

module type S = sig
  type t [@@deriving sexp_of]

  val sampler : t Sampler.t
end

module Test = struct
  type t =
    | T :
        { module_ : (module S with type t = 'a)
        ; f : 'a -> unit
        }
        -> t

  let create (type a) (module S : S with type t = a) ~f = T { module_ = (module S); f }
end

let afl_command tests =
  let tests = Array.of_list tests in
  Command.basic ~summary:"Run AFL on tests"
  @@
  let%map_open.Command input_file = anon ("FILE" %: Filename_unix.arg_type) in
  fun () ->
    AflPersistent.run (fun () ->
      In_channel.with_file input_file ~f:(fun in_channel ->
        let state = State.create_in_channel ~in_channel in
        let (Test.T { module_; f }) = State.choose state tests in
        let module S = (val module_) in
        let input = Sampler.sample S.sampler state in
        try f input with
        | exn ->
          let input = Sexp.to_string [%sexp (input : S.t)] in
          Exn.reraise exn [%string "Test raised with the following input: %{input}"]))
;;

let quickcheck_command tests =
  let tests = Array.of_list tests in
  Command.basic ~summary:"Run quickcheck on tests"
  @@
  let%map_open.Command iterations =
    flag "iterations" (optional_with_default 10000 int) ~doc:"NUM number of iterations"
  in
  fun () ->
    let state = State.create_random ~seed:[| 0 |] in
    for _ = 1 to iterations do
      let (Test.T { module_; f }) = State.choose state tests in
      let module S = (val module_) in
      let input = Sampler.sample S.sampler state in
      try f input with
      | exn ->
        let input = Sexp.to_string [%sexp (input : S.t)] in
        Exn.reraise exn [%string "Test raised with the following input: %{input}"]
    done
;;

let create_command tests =
  Command.group
    ~summary:"Run tests"
    [ "afl", afl_command tests; "quickcheck", quickcheck_command tests ]
;;

let%expect_test "" = ()
