open Expr_ds

module Subst :
sig
  type subst = Expr_ds.fp_assignment
  type substs = subst list

  val domain : substs -> formal_param list
  val range : term_db -> substs -> op_decl list
  val formal_params_in_range : term_db -> substs -> formal_param list

  (** rename a formal parameter to a name different from each of the arguments
      free, bound and defs *)
  val rename : term_db -> ?free:(op_decl list) -> ?bound:(formal_param list)
    -> ?defs:(op_def list) -> formal_param -> (term_db * formal_param)

  (** Looks for mapping of fp in substs. Lookups are done in the termdb.
      The default comparator is structural equality.
  *)
  val find_subst : ?cmp:(formal_param_ -> formal_param_ -> bool) ->
    term_db -> formal_param -> substs -> expr_or_op_arg option

  (** Removes all passed formal parameters from a substition *)
  val remove_from_subst : term_db -> formal_param list -> substs -> substs
end

module SubFormat :
sig
  (** Formatter for a single mapping x <- t *)
  val fmt_subst : Expr_ds.term_db -> Format.formatter -> Subst.subst -> unit

  (** Formatter for a substitution x <- s, y <- t *)
  val fmt_substs :
    ?intro:string ->
    Expr_ds.term_db -> Format.formatter -> Subst.subst list -> unit

  val fmt_renaming :
    ?intro:string ->
    Expr_ds.term_db ->
    Format.formatter ->
    (Expr_ds.formal_param * Expr_ds.formal_param) list -> unit
end

val subst_expr : term_db -> Subst.substs -> expr -> expr * term_db
val subst_op   : term_db -> Subst.substs -> operator -> operator * term_db
