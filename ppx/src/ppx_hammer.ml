open! Base
open! Ppxlib
open! Ppx_hammer_expander

let () = Deriving.add "hammer" ~str_type_decl ~sig_type_decl |> Deriving.ignore
