open! Core

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

let int_incl t lo hi =
  assert (lo <= hi);
  if lo = hi then lo else (int t mod (hi - lo + 1)) + lo
;;

let int32 =
  let buf = Bytes.create 4 in
  function
  | Random state -> Random.State.int32 state Int32.max_value
  | In_channel in_channel ->
    (match In_channel.really_input in_channel ~buf ~pos:0 ~len:4 with
     | None -> raise Eof
     | Some () -> Bytes.unsafe_get_int32 buf 0)
;;

let int64 =
  let buf = Bytes.create 8 in
  function
  | Random state -> Random.State.int64 state Int64.max_value
  | In_channel in_channel ->
    (match In_channel.really_input in_channel ~buf ~pos:0 ~len:8 with
     | None -> raise Eof
     | Some () -> Bytes.unsafe_get_int64 buf 0)
;;

let nativeint t = int t |> Nativeint.of_int

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

let string t ~size = String.init size ~f:(fun _ -> char t)

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
