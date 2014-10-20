(*
 * Copyright (C) 2011  INRIA and Microsoft Corporation
 *)

open Types
open Obligations

(* backend/isabelle.ml *)
val toolbox_print :
  obligation ->
  ?temp:bool ->
  string ->
  string option ->
  string option ->
  float ->
  bool option ->
  bool ->
  reason option ->
  string ->
  float option ->
    unit
;;

(* backend/prep.ml *)
val print_new_res :
  obligation -> status_type6 -> string -> float option -> unit
;;
val print_message : string -> unit;;
val print_old_res : obligation -> status_type6 -> bool -> unit;;

(* tlapm.ml *)
val print_ob_number : int -> unit;;
val print_message_url : string -> string -> unit;;
