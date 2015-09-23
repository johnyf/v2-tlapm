open Commons
open Expr_ds
open Expr_formatter
open Format
open Obligation

let fmt_obligation pp { goal; expanded_defs; provers; term_db;
                        constants; variables; definitions;
                        assumptions; theorems; } =
  let acc = (pp, term_db, false, Expression, 0) in
  ignore (expr_formatter#assume_prove acc goal)
