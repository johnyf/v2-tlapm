open Commons
open Simple_expr_ds
open Simple_expr_formatter
open Format
open Simple_obligation

let fmt_obligation pp { goal; expanded_defs;
			provers; term_db; } =
  let acc = (pp, term_db, false, Expression, 0) in
  ignore (expr_formatter#assume_prove acc goal; print_flush () )
