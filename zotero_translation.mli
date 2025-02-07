(** {1 Interface to the Zotero Translation Server} *)

type t

type format =
  | Bibtex
  | Biblatex
  | Bookmarks
  | Coins
  | Csljson
  | Csv
  | Endnote_xml
  | Evernote
  | Mods
  | Rdf_bibliontology
  | Rdf_dc
  | Rdf_zotero
  | Refer
  | Refworks_tagged
  | Ris
  | Tei
  | Wikipedia

val format_to_string: format -> string
val format_of_string: string -> format option

val v :  string -> t

val resolve_doi: t -> string -> ([>Ezjsonm.t], [>`Msg of string]) Lwt_result.t

val search_id: t -> string -> ([>Ezjsonm.t], [>`Msg of string]) Lwt_result.t

val export: t -> format -> Ezjsonm.t -> (string, [>`Msg of string]) Lwt_result.t
