open! Core

let%expect_test "Dump sexp representation of hello world program" =
  {| let () = print_endline "Hello, world!";; |}
  |> Stdlib.Lexing.from_string
  |> Ppxlib.Parse.implementation
  |> Astlib.Migrate_500_501.copy_structure
  |> [%sexp_of: Parsetree_hammer.Parsetree_501.structure]
  |> print_s;
  [%expect
    {|
    (((pstr_desc
       (Pstr_value Nonrecursive
        (((pvb_pat
           ((ppat_desc
             (Ppat_construct
              ((txt (Lident "()"))
               (loc ((loc_start :1:5) (loc_end :1:7) (loc_ghost false))))
              ()))
            (ppat_loc ((loc_start :1:5) (loc_end :1:7) (loc_ghost false)))
            (ppat_loc_stack ()) (ppat_attributes ())))
          (pvb_expr
           ((pexp_desc
             (Pexp_apply
              ((pexp_desc
                (Pexp_ident
                 ((txt (Lident print_endline))
                  (loc ((loc_start :1:10) (loc_end :1:23) (loc_ghost false))))))
               (pexp_loc ((loc_start :1:10) (loc_end :1:23) (loc_ghost false)))
               (pexp_loc_stack ()) (pexp_attributes ()))
              ((Nolabel
                ((pexp_desc
                  (Pexp_constant
                   (Pconst_string "Hello, world!"
                    ((loc_start :1:25) (loc_end :1:38) (loc_ghost false)) ())))
                 (pexp_loc ((loc_start :1:24) (loc_end :1:39) (loc_ghost false)))
                 (pexp_loc_stack ()) (pexp_attributes ()))))))
            (pexp_loc ((loc_start :1:10) (loc_end :1:39) (loc_ghost false)))
            (pexp_loc_stack ()) (pexp_attributes ())))
          (pvb_constraint ()) (pvb_attributes ())
          (pvb_loc ((loc_start :1:1) (loc_end :1:39) (loc_ghost false)))))))
      (pstr_loc ((loc_start :1:1) (loc_end :1:39) (loc_ghost false))))) |}]
;;
