(*
 * util.mli --- format utilities
 *
 *
 * Copyright (C) 2008-2010  INRIA and Microsoft Corporation
 *)

(** A collection of utilities *)

open Property

(** {3 Locations} *)

val locate      : 'a -> Loc.locus -> 'a wrapped
val location    : ?cap:bool -> 'a wrapped -> string

val get_locus   : 'a wrapped -> Loc.locus
val query_locus : 'a wrapped -> Loc.locus option
val set_locus   : 'a wrapped -> Loc.locus -> 'a wrapped

(** {3 Hinting and collections of hints} *)

type hint = string wrapped

val pp_print_hint : Format.formatter -> hint -> unit

module Coll : sig
  module Sm : Map.S with type key = string
  module Hm : Map.S with type key = hint

  module Ss : Set.S with type elt = string
  module Hs : Set.S with type elt = hint

  module Sh : Weak.S with type data = string
end

(** {3 Printing with locations} *)

val sprintf :
  ?debug:string -> ?at:('a wrapped) -> ?prefix:string -> ?nonl:unit ->
  ('r, Format.formatter, unit, string) Pervasives.format4 -> 'r
val printf  :
  ?debug:string -> ?at:('a wrapped) -> ?prefix:string -> ?nonl:unit ->
  ('r, Format.formatter, unit) Pervasives.format -> 'r
val eprintf :
  ?debug:string -> ?at:('a wrapped) -> ?prefix:string -> ?nonl:unit ->
  ('r, Format.formatter, unit) Pervasives.format -> 'r
val fprintf :
  ?debug:string -> ?at:('a wrapped) -> ?prefix:string -> ?nonl:unit ->
  Format.formatter ->
  ('r, Format.formatter, unit) Pervasives.format -> 'r

(** {3 Convenience functions for exceptions} *)

exception Bug
val bug : ?at:('a wrapped) -> string -> 'failure

(** {3 File and object checksumming} *)

type csum
val checksum : string -> csum
val pp_print_csum : Format.formatter -> csum -> unit

(** {3 Text wrangling} *)

val plural : ?ending:string -> int -> string -> string
val line_wrap : ?cols:int -> string -> string

(** {3 Misc} *)

val heap_stats : unit -> unit
val temp_file :
  (unit -> unit) ref -> string -> string * out_channel
;;
(** [temp_file clean_hook suffix]
    Create a temporary file, return its name and an output channel to it.
    Add to [clean_hook] a clean-up action that closes the out_channel
    and removes the file.
*)

val rm_temp_file : string -> unit
(** Remove the given temporary file unless debugging "tempfiles" *)

val add_hook : (unit -> unit) ref -> ('a -> unit) -> 'a -> unit;;
(** [add_hook cleanup fn argument]
    Adds to [cleanup] the action of calling [fn argument] before doing
    whatever was in [cleanup] already.
*)


(** {3 General functions } *)

val ( @$ ) : ('a -> 'b) -> 'a -> 'b
(**
  Right-associative function application. You can write
  'hd @$ tl @$ tl @$ [1;2;3;4;5]' instead of 'hd ( tl ( tl ( [1;2;3;4;5] )))'.
  @see <http://blog.0branch.com/posts/2012-04-17-haskell-application-ocaml.html>
      Original Source
*)

(** {3 List utils } *)

(** [find_ordering pairs] *)
val find_ordering : ('a * 'a) list -> 'a list
(** given a list of pairs, where the first argument is considered as less
    than the second argument, find an ordering of the elements which
    satisfies the transitive closure of the relation input. (I.e. find a
    linear extension of pairs)
*)

(** [add_missing list list2] *)
val add_missing : 'a list -> 'a list -> 'a list
(** Appends list2 to list without creating duplicates. Does not remove
    duplicates from list.
*)

(** Compares two lists for equivalence and treats them as multi-sets. I.e. *)
val multiset_equal_lists : 'a list -> 'a list -> bool

(** {3 String utils} *)
(** [mkString ~front ~middle ~back fmt list ] *)
val mkString : ?front:string -> ?middle:string -> ?back:string  ->
	       ( 'a -> string) -> 'a list -> string
(** Creates a string from the given
    list by mapping fmt on each element, using ~middle as a seperator. The
    string ~front is prepended, while ~back is appended.
 *)

(** [mkString ~front ~middle ~back fmt list ] *)
val fmtPair : ?front:string -> ?middle:string -> ?back:string  ->
	      ('a -> string) -> ('b -> string) -> ('a * 'b) -> string
(** Creates a string from the given
    list by mapping fmt on each element, using ~middle as a seperator. The
    string ~front is prepended, while ~back is appended.
*)
