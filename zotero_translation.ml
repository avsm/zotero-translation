(** Resolve a DOI from a Zotero translation server *)

module C = Cohttp
module CL = Cohttp_lwt
module CLU = Cohttp_lwt_unix.Client

(* From the ZTS source code:  https://github.com/zotero/translation-server/blob/master/src/formats.js
  bibtex: "9cb70025-a888-4a29-a210-93ec52da40d4",
	biblatex: "b6e39b57-8942-4d11-8259-342c46ce395f",
	bookmarks: "4e7119e0-02be-4848-86ef-79a64185aad8",
	coins: "05d07af9-105a-4572-99f6-a8e231c0daef",
	csljson: "bc03b4fe-436d-4a1f-ba59-de4d2d7a63f7",
	csv: "25f4c5e2-d790-4daa-a667-797619c7e2f2",
	endnote_xml: "eb7059a4-35ec-4961-a915-3cf58eb9784b",
	evernote: "18dd188a-9afc-4cd6-8775-1980c3ce0fbf",
	mods: "0e2235e7-babf-413c-9acf-f27cce5f059c",
	rdf_bibliontology: "14763d25-8ba0-45df-8f52-b8d1108e7ac9",
	rdf_dc: "6e372642-ed9d-4934-b5d1-c11ac758ebb7",
	rdf_zotero: "14763d24-8ba0-45df-8f52-b8d1108e7ac9",
	refer: "881f60f2-0802-411a-9228-ce5f47b64c7d",
	refworks_tagged: "1a3506da-a303-4b0a-a1cd-f216e6138d86",
	ris: "32d59d2d-b65a-4da4-b0a3-bdd3cfb979e7",
	tei: "032ae9b7-ab90-9205-a479-baf81f49184a",
	wikipedia: "3f50aaac-7acc-4350-acd0-59cb77faf620"
 *)
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

let format_to_string = function
  | Bibtex -> "bibtex"
  | Biblatex -> "biblatex"
  | Bookmarks -> "bookmarks"
  | Coins -> "coins"
  | Csljson -> "csljson"
  | Csv -> "csv"
  | Endnote_xml -> "endnote_xml"
  | Evernote -> "evernote"
  | Mods -> "mods"
  | Rdf_bibliontology -> "rdf_bibliontology"
  | Rdf_dc -> "rdf_dc"
  | Rdf_zotero -> "rdf_zotero"
  | Refer -> "refer"
  | Refworks_tagged -> "refworks_tagged"
  | Ris -> "ris"
  | Tei -> "tei"
  | Wikipedia -> "wikipedia"

let format_of_string = function
  | "bibtex" -> Some Bibtex
  | "biblatex" -> Some Biblatex
  | "bookmarks" -> Some Bookmarks
  |  "coins" -> Some Coins
  | "csljson" -> Some Csljson
  | "csv" -> Some Csv
  | "endnote_xml" -> Some Endnote_xml
  | "evernote" -> Some Evernote
  | "mods" -> Some Mods
  | "rdf_bibliontology" -> Some Rdf_bibliontology
  | "rdf_dc" -> Some Rdf_dc
  | "rdf_zotero" -> Some Rdf_zotero
  | "refer" -> Some Refer
  | "refworks_tagged" -> Some Refworks_tagged
  | "ris" -> Some Ris
  | "tei" -> Some Tei
  | "wikipedia" -> Some Wikipedia
  | _ -> None

let web_endp base_uri =
  match String.ends_with ~suffix:"/" base_uri with
  | true -> Uri.of_string (base_uri ^ "web")
  | false -> Uri.of_string (base_uri ^ "/web")

let export_endp base_uri =
  match String.ends_with ~suffix:"/" base_uri with
  | true -> Uri.of_string (base_uri ^ "export")
  | false -> Uri.of_string (base_uri ^ "/export")

let search_endp base_uri =
  match String.ends_with ~suffix:"/" base_uri with
  | true -> Uri.of_string (base_uri ^ "search")
  | false -> Uri.of_string (base_uri ^ "/search")

let _import_endp base_uri =
  match String.ends_with ~suffix:"/" base_uri with
  | true -> Uri.of_string (base_uri ^ "import")
  | false -> Uri.of_string (base_uri ^ "/import")

open Lwt.Infix

(* The Eio version has more in here, hence I'm just keeping this around. *)
type t = {
  base_uri: string;
}

let v base_uri = { base_uri }

let resolve_doi { base_uri } doi =
  let body = "https://doi.org/" ^ doi in
  let doi_body = CL.Body.of_string body in
  let headers = Http.Header.init_with "content-type" "text/plain" in
  let uri = web_endp base_uri in
  CLU.call ~headers ~body:doi_body `POST uri >>= fun (resp, body) ->
  let status = C.Response.status resp in
  body |> Cohttp_lwt.Body.to_string >>= fun body ->
  if status = `OK then begin
    try
      let doi_json = Ezjsonm.from_string body in
      Lwt.return_ok doi_json
    with exn -> Lwt.return_error (`Msg (Printexc.to_string exn))
  end else
    Lwt.return_error (`Msg (Format.asprintf "Unexpected HTTP status: %a for %s" Http.Status.pp status body))

let search_id { base_uri} doi =
  let body = "https://doi.org/" ^ doi in
  let doi_body = CL.Body.of_string body in
  let headers = Http.Header.init_with "content-type" "text/plain" in
  let uri = search_endp base_uri in
  CLU.call ~headers ~body:doi_body `POST uri >>= fun (resp, body) ->
  let status = C.Response.status resp in
  body |> Cohttp_lwt.Body.to_string >>= fun body ->
  if status = `OK then begin
      try
        let doi_json = Ezjsonm.from_string body in
        Lwt.return_ok doi_json
      with exn -> Lwt.return_error (`Msg (Printexc.to_string exn))
  end else
    Lwt.return_error (`Msg (Format.asprintf "Unexpected HTTP status: %a for %s" Http.Status.pp status body))

let export {base_uri} format api =
  let body = CL.Body.of_string (Ezjsonm.to_string api) in
  let headers = Http.Header.init_with "content-type" "application/json" in
  let uri = Uri.with_query' (export_endp base_uri ) ["format", (format_to_string format)] in
  CLU.call ~headers ~body `POST uri >>= fun (resp, body) ->
    let status = C.Response.status resp in
    body |> Cohttp_lwt.Body.to_string >>= fun body ->
      if status = `OK then begin
          try
            match format with
            | Bibtex -> Lwt.return_ok (Astring.String.trim body)
            | _ -> Lwt.return_ok body
          with exn -> Lwt.return_error (`Msg (Printexc.to_string exn))
      end else
        Lwt.return_error (`Msg (Format.asprintf "Unexpected HTTP status: %a for %s" Http.Status.pp status body))
    