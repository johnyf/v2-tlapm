open Commons
open Expr_ds
open Expr_builtins

type fixity =
  | Standard
  | UnaryPrefix
  | UnaryPostfix
  | BinaryInfix
  | Special of Builtin.builtin_symbol

val match_constant : term_db -> expr -> string option

val match_function : term_db -> expr -> (string * expr_or_op_arg list) option

val expr_to_prover : term_db -> expr -> prover option

(* val match_prefix_op : term_db -> operator ->
                     (string * formal_param * formal_param) option
*)
val match_infix_op : term_db -> operator -> bool

val match_ternary_op : term_db -> operator -> string option

val extract_fixity : string -> 'a list -> fixity
val match_fixity_op : term_db -> operator -> fixity

val prefix_names : string list
val infix_names : string list
val postfix_names : string list
val ternary_names : string list

val expand_ternary_name : string -> string * string * string
