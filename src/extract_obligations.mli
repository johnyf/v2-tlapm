open Commons
open Expr_ds
open Expr_visitor
open Obligation

(* used to track the nesting level throughout a proof *)
type nesting =
  | Module
  | Expression
  | ProofStep
  | By

type current_context = {
(* the current goal *)
goal : assume_prove option;

(* the facts currently known *)
usable_facts : assume_prove list;

(* the expanded definitions *)
expanded_defs : op_def list;

(* the term database *)
term_db : term_db;

(* visible theorems etc.  not sure if we need that *)
constants         : op_decl list;
variables         : op_decl list;
definitions       : op_def list ;
assumptions       : assume list ;
theorems          : theorem list ;
}

type 'a eoacc = current_context * obligation list * nesting * 'a

val emptyCurrentContext : term_db -> current_context
(* val extract_obligation_from_module *)

class ['a] extract_obligations :
object
inherit ['a eoacc] visitor
end
