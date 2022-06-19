(lang dune 3.2)

(name textgridaf)

(generate_opam_files true)

(source
 (github dvmitrv/textgridaf))

(authors "Vlad Dumitru <vlad@dumitru.net>")

(maintainers "Vlad Dumitru <vlad@dumitru.net>")

(license MIT)

(package
 (name textgridaf)
 (synopsis "Praat TextGrid parser and serializer")
 (description "Parser and serializer for Praat TextGrid files, built with Angstrom and Faraday")
 (depends ocaml dune angstrom faraday jsonaf uutf)
 (tags
  (praat textgrid parser serializer)))
