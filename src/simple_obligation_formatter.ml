open Commons
open Simple_expr_ds
open Format
open Simple_obligation
       
let fmt_obligation pp { goal; term_db; } =
  let acc = (pp, term_db, false, Simple_expr_formatter.Expression, 0) in
  ignore(Simple_expr_formatter.expr_formatter#assume_prove acc goal);
  print_flush ()


let fmt_nunchaku pp { goal; term_db; } =
  let sta = Nunchaku_formatter.fmt_assume_prove term_db goal in
  ignore(Nunchaku_statement.print_statement pp sta)
