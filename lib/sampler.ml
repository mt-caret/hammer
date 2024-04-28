open! Core

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

  let size =
    create (fun state ->
      let rec go accum =
        let accum = accum * 255 in
        match State.char state |> Char.to_int with
        | 255 -> go (accum + State.int_incl state 0 254)
        | n (* 0 - 254 *) -> accum + n
      in
      go 0)
  ;;

  let sampler_int = create State.int
  let sampler_int32 = create State.int32
  let sampler_int64 = create State.int64
  let sampler_nativeint = create State.nativeint
  let sampler_char = create State.char
  let sampler_bool = create State.bool

  let sampler_string =
    create (fun state ->
      let size = sample size state in
      State.string state ~size)
  ;;

  let sampler_option sampler_a =
    create (fun state ->
      if sample sampler_bool state then None else Some (sample sampler_a state))
  ;;

  let sampler_list sampler_a =
    create (fun state ->
      let size = sample size state in
      List.init size ~f:(fun _ -> sample sampler_a state))
  ;;

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
