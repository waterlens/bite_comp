(lang dune 3.3)

(name bite_comp)

(generate_opam_files true)

(source
 (github waterlens/bite))

(authors "Waterlens")

(maintainers "Waterlens")

(license MIT)

(documentation https://github.com/waterlens/bite_comp)

(using menhir 2.1)

(package
 (name bite_comp)
 (synopsis "A compiler for experimental purpose")
 (depends ocaml dune llvm llvm.irreader llvm.linker)
 (tags
  (lang)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
