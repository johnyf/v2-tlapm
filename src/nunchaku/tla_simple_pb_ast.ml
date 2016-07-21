open Expr_simple
open Obligation
open Simple_expr_ds

(** Definition **)
       
type simple_obligation = {
    (* actual obligation, without expansion *)
    goal : simple_assume_prove;

    (* the term database *)
    term_db : simple_term_db;
}


(** Translation **)
                           
let obligation_to_simple_obligation (obl:obligation) =
  let (stermdb, ap) = parse_expr obl.term_db obl.goal in
  {goal = ap; term_db = stermdb;}

(** Printer **)
