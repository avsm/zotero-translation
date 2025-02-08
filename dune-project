(lang dune 3.17)
(name zotero-translation)

(source (github avsm/zotero-translation))
(license ISC)
(authors "Anil Madhavapeddy")
(maintainers "anil@recoil.org")

(generate_opam_files true)

(package
 (name zotero-translation)
 (synopsis "API client to the Zotero translation server")
 (description "This is all still a work in progress")
 (depends
  (ocaml (>= "5.1.0"))
  uri
  cohttp-lwt-unix
  ezjsonm
  yaml))
