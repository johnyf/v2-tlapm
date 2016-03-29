open Commons
open Simple_expr_ds

type obligation_type =
  | Formula
  | Suffices
  | Have
  | Take
  | Witness
  | Qed

type simple_obligation = {
(* actual obligation, without expansion *)
goal : simple_assume_prove;

(* the term database *)
term_db : simple_term_db;

}
