(lang dune 3.15)

(name hammer)

(generate_opam_files true)

(source
 (github mt-caret/hammer))

(authors "mtakeda")

(maintainers "mtakeda")

(license MIT)

(documentation https://github.com/mt-caret/hammer)

(package
 (name hammer)
 (synopsis "A library for fuzzing using quickcheck-style tests")
 (description
  "A library for fuzzing using quickcheck-style tests, inspired by stedolan/crowbar")
 (depends ocaml dune afl-persistent core core_unix ppx_jane)
 (tags (testing)))
