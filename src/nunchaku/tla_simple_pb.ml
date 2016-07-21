open Format
open Expr_simple
open Tla_pb
open Simple_expr_ds

(** Definition **)
       
type tla_simple_pb = {
    (* actual obligation, without expansion *)
    goal : simple_assume_prove;

    (* the term database *)
    term_db : simple_term_db;
}


(** Translation **)
                           
let tla_pb_to_tla_simple_pb (tla_pb:tla_pb) =
  let (stermdb, ap) = parse_expr tla_pb.term_db tla_pb.goal in
  {goal = ap; term_db = stermdb;}

    
(** Printer **)

let fmt_tla_simple_pb pp { goal; term_db; } =
  let acc = (pp, term_db, false, Simple_expr_formatter.Expression, 0) in
  ignore(Simple_expr_formatter.expr_formatter#assume_prove acc goal);
  print_flush ()

