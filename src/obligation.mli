open Commons
open Expr_ds

type obligation_type =
  | Formula
  | Suffices
  | Have
  | Take
  | Witness
  | Qed

type obligation = {
(* actual obligation, without expansion *)
goal : assume_prove;

(* the expanded definitions *)
expanded_defs : op_def list;

(* the backend provers to handle the obligation *)
provers : prover list;

(* the term database *)
term_db : term_db;

(* visible theorems etc.  not sure if we need that *)
constants         : op_decl list;
variables         : op_decl list;
definitions       : op_def list ;
assumptions       : assume list ;
theorems          : theorem list ;

}
