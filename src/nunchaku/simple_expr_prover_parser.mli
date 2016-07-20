open Commons
open Simple_expr_ds

val match_constant : simple_term_db -> simple_expr -> string option

val match_function : simple_term_db -> simple_expr -> (string * simple_expr_or_op_arg list) option

val expr_to_prover : simple_term_db -> simple_expr -> prover option

(* val match_prefix_op : simple_term_db -> operator ->
                     (string * formal_param * formal_param) option
 *)
val match_infix_op : simple_term_db -> simple_operator -> bool

val match_ternary_op : simple_term_db -> simple_operator -> string option

val prefix_names : string list
val infix_names : string list
(* val postfix_names : string list *)
val ternary_names : string list

val expand_ternary_name : string -> string * string * string
