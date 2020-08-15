(lang dune 2.0)
(generate_opam_files true)

(name ocaml_webapp)
(version 0.1.0)

(package
 (name ocaml_webapp)
 (depends
  ;; General system dependencies
  (dune (>= 2))
  (ocaml (>= 4.09.0))
  (yojson (>= v1.7.0))

  ;; Standard library replacement
  (core (>= v0.12.2))

  ;; Web toolkit
  (opium (>= 0.17.1))

  ;; Database interface
  (caqti (>= 1.2.1))
  (caqti-lwt (>= 1.2.0))
  (caqti-driver-postgresql (>= 1.2.1))
  (ppx_rapper (>= 0.9.2))

  ;; Logging
  (logs (>= 0.7.0))

  ;; Dev dependencies
  (utop :dev)
  (merlin :dev)
  (ocamlformat :dev)
  (ocp-indent :dev)
))
