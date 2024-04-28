open! Core
include Hammer_intf
module State = State
module Sampler = Sampler

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
