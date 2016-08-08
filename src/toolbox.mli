open Expr_ds
open Commons
open Format

(* This implements the datastructures for the toolbox communication protocol.
   See also general/docs/tlapm-toolbox-interface.txt in the toolbox code.
*)

type toolbox_status = | ToBeProved | Proved | Failed | Trivial
                      | BeingProved | Interrupted

(* TODO: how many proofs actually use that? *)
type toolbox_method = | Auto | Blast | Force | Fail | Sorry

type toolbox_msg = {
  id       : int;                      (* obligation id *)
  location : location;                 (* location of statement to prove *)
  status   : toolbox_status;           (* proving status of the obligation *)
  prover   : prover option;            (* which prover was used *)
  meth     : toolbox_method option;    (* which method was passed *)
  already_processed : bool option;     (* fingerprint used *)
  obligation_string  : string option;
}

val fmt_toolbox_status : formatter -> toolbox_status -> unit
(** Formatter for a toolbox_status. *)

val fmt_toolbox_method : formatter -> toolbox_method -> unit
(** Formatter for a toolbox_method. *)

val fmt_toolbox_msg    : formatter -> toolbox_msg -> unit
(** Formatter for a toolbox_msg. Implements the type:obligation toolbox message. *)

val fmt_toolbox_msg_d  : ?prover:prover -> ?meth:toolbox_method ->
  ?already_processed:bool -> ?obligation_string:string ->
  formatter -> int -> location -> toolbox_status -> unit
(** Alternative formatter to fmt_toolbox_msg without the need to create the record. *)

val fmt_toolbox_msg_count : formatter -> int -> unit
(** Formatter, implements the type:obligationsnumber toolbox message. *)
