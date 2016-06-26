(*
 * util.mli --- format utilities
 *
 *
 * Copyright (C) 2008-2010  INRIA and Microsoft Corporation
 *)
open Format
type 'a fmt = formatter -> 'a -> unit

(** A collection of utilities *)


(** {3 Convenience functions for exceptions} *)

exception Bug
(* val bug : ?at:('a wrapped) -> string -> 'failure *)

(** {3 File and object checksumming} *)

type csum
val checksum : string -> csum
val pp_print_csum : Format.formatter -> csum -> unit

(** {3 Text wrangling} *)

val plural : ?ending:string -> int -> string -> string
val line_wrap : ?cols:int -> string -> string

(** {3 Misc} *)

val heap_stats : unit -> unit

(*
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

*)
                           
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
val flat_map : ('a -> 'b list) -> 'a list -> 'b list
(** The composition of map and flatten. *)

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
val fmt_pair : ?front:string -> ?middle:string -> ?back:string  ->
              ('a  fmt) -> ('b fmt) -> formatter -> ('a * 'b) -> unit

(** fmt_pair ~front ~middle ~back fmt_a fmt_b formatter (a,b) prints
    the pair (a,b) with the formatters fmt_a and fmt_b. They optional
    arguments ~front, ~middle and ~back are printed at the respective positions.
*)


val fmt_option : ?none:string -> ?some:string -> ?some_back:string -> ('a  fmt)
                 -> formatter -> 'a option -> unit

(** fmt_option ~none ~somefmt_a fmt_b formatter (a,b) prints
    the option with the formatter fmt_a. The optional
    arguments ~none and ~some are printed for the respective cases.
    The optional argument ~some_back is printed at the end of the some case.
*)


val fmt_string : formatter -> string -> unit
(** fmt_string formatter string prints the given string to the formatter.
 *)

val fmt_list : ?front:string -> ?middle:string -> ?back:string  ->
               ( 'a fmt) -> formatter -> 'a list -> unit
(** fmt_list ~front ~middle ~back fmt_elem f l prints each
    element of the list l to the given formatter using the element formatter
    fmt_elem. First the string ~front is printed, elements are seperated by
    ~middle and ~back closes the list.
 *)
