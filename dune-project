(lang dune 3.24)

(name trip_tab)

(generate_opam_files true)

(source
 (github ukaylee/cs3110-team5-project))

(authors
 "Kaylee Ulep" "Amy Yang" "Jennifer Ng" "Elizabeth Chen" "Sandy Yang")

(maintainers  "Kaylee Ulep" "Amy Yang" "Jennifer Ng" "Elizabeth Chen" "Sandy Yang")

(license LICENSE)

(package
 (name trip_tab)
 (allow_empty)
 (synopsis "Terminal app for splitting group trip expenses")
 (description
  "Logs shared expenses, splits them equally, by amount, by percentage, or itemized, computes minimal settle-up payments, and tracks budgets.")
 (depends ocaml dune ounit2)
 (tags
  ("ocaml" "expenses" "cs3110")))