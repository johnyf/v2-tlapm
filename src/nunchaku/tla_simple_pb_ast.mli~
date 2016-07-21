open Commons
open Simple_expr_ds

type simple_obligation = {
(* actual obligation, without expansion *)
goal : simple_assume_prove;

(* the term database *)
term_db : simple_term_db;
}

val make_simple_obligation : simple_assume_prove -> simple_term_db -> simple_obligation
			   
val obligation_to_simple_obligation : Obligation.obligation -> simple_obligation
