open Obligation
open Nunchaku_ast

type nunchaku_result = Mod.mod_tree

val nunchaku_result_printer : nunchaku_result -> string
       
(* val print_simple : obligation list -> string -> unit *)

(* val print_complex : obligation list -> string -> unit *)

(* val print_nunchaku : obligation list -> string -> int *)

val call_nunchaku : statement list -> Sexplib.Type.t

val nunchaku : 'a -> obligation -> Mod.mod_tree
