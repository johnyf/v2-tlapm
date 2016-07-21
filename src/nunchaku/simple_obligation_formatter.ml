open Commons
open Simple_expr_ds
open Format
open Tla_simple_pb_ast
       
let fmt_obligation pp { goal; term_db; } =
  let acc = (pp, term_db, false, Simple_expr_formatter.Expression, 0) in
  ignore(Simple_expr_formatter.expr_formatter#assume_prove acc goal);
  print_flush ()
