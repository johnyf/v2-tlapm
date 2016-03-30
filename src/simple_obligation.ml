open Expr_simple
open Obligation
open Simple_expr_ds

type simple_obligation = {
(* actual obligation, without expansion *)
goal : simple_assume_prove;

(* the term database *)
term_db : simple_term_db;
}
       
let make_simple_obligation g tdb : simple_obligation =
  {goal = g ; term_db = tdb}
     
let obligation_to_simple_obligation (obl:obligation) =
  let (stermdb, ap) = parse_expr obl.term_db obl.goal in
  make_simple_obligation ap stermdb  
  
