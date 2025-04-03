(*---------------------------------------------------------------------------
   Copyright (c) 2019 University of Bern. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** {{:https://www.ctan.org/pkg/bibtex}BibT{_E}X} codec.

    {b Limitations.} At the moment [@string], [@preamble]
    and [@comment] are not supported. For values we assume UTF-8 without
    escape sequences. Nested braces are handled though. *)

val escape : string -> string
(** [escape s] escapes [s] for BibT{_E}X. *)

type t
(** The type for bibtex entries. *)

module Tloc : sig
  type t
end

module SM : Map.S with type key := string

val v :
  type':string -> cite_key:string -> fields:string SM.t -> unit -> t
(** [v ~type' ~id ~fields] is an entry of type [type'], identifier [id],
    and with field [fields]. *)

val type' : t -> string
(** [type' e] is the type of entry. *)

val cite_key : t -> string
(** [cite_key e] is the citation key of the entry. *)

val fields : t -> string SM.t
(** [fields e] are the BibTeX fields. Fields are lowercased according
    to {!B0_std.String.Ascii.lowercase}. *)

val pp : t Fmt.t
(** [pp] formats an entry using BibT{_E}X syntax. *)

(** {1:fields Field queries} *)

val list_value : string -> string list
(** [list_value] splits on comma and trims the results. *)

val doi : t -> string option
(** [doi e] is the [doi] field of e. Note that if the field happens to
    hold an URI, the scheme and authority are stripped. *)

val keywords : t -> string list option
(** [keywords e] is the comma seperated [keywords] field. *)

val annote : t -> string option
(** [annote e] is the [annote] field. *)

(** {1:codec Codec} *)

type error_kind
(** The type for kinds of decoding errors. *)

type error = error_kind * Tloc.t
(** The type for errors. The error and its location. *)

val pp_error : error Fmt.t

val of_string : ?file:Fpath.t -> string -> (t list, error) result
(** [of_string ~file s] parses entries from [s] assuming it
    was read from [file] (defaults to {!B0_std.Fpath.dash}). *)

val of_string' : ?file:Fpath.t -> string -> (t list, string) result
(** [of_string'] is like {!of_string} but converts the error to an
    error message. *)

val to_string : t list -> string
(** [to_string es] formats the list of entries using BibT{_E}X syntax. *)

(*---------------------------------------------------------------------------
   Copyright (c) 2019 University of Bern

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
