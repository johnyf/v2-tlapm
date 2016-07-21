open Obligation
open Nun_pb_ast
open Nunchaku_formatter
open Nun_sexp_ast
open Expr_simple
open Tla_pb
open Tla_simple_pb
       
type nunchaku_result = Nun_mod_ast.mod_tree

(* val obligation_to_simple_obligation : obligation -> simple_obligation *)
                         
val nunchaku_result_printer : nunchaku_result -> string
       
(* val print_simple : obligation list -> string -> unit *)

(* val print_complex : obligation list -> string -> unit *)

(* val print_nunchaku : obligation list -> string -> int *)

val call_nunchaku : statement list -> Sexplib.Type.t

val nunchaku : 'a -> tla_pb -> Nun_mod_ast.mod_tree
