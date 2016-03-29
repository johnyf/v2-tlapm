open Expr_simple
open Obligation
open Simple_obligation
       
let obligation_to_simple_obligation { goal; expanded_defs; provers; term_db;
                        constants; variables; definitions;
                        assumptions; theorems; } =
  let pars = new expr_to_simple_expr in
  pars#assume_prove goal goal

