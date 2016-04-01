open Expr_ds

module Subst :
sig
  type subst = Subst of formal_param * expr
  type substs = subst list

  val domain : substs -> formal_param list

  (*  val range : substs -> op_decl list *)

  (** rename a formal parameter to a name different from each of the arguments
      free, bound and defs *)
  val rename : term_db -> ?free:(op_decl list) -> ?bound:(formal_param list)
               -> ?defs:(op_def list) -> formal_param -> (term_db * formal_param)

  (** Looks for mapping of fp in substs. Lookups are done in the termdb. 
      The default comparator is structural equality.
   *)
  val find_subst : ?cmp:(formal_param_ -> formal_param_ -> bool) ->
                   term_db -> formal_param -> substs -> expr option
end
