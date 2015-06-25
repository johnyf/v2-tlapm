open Commons
open Expr_ds
open Expr_visitor
open Any_expr

(** A transformer template for expressions. It is a visitor with a special
    accumulator of type macc, a tuple. The first element of macc is an anyExpr
    which represents the transformed elemented of what is visited. The second
    element is an accumulator for derived classes to pass on information.

    The reason to use anyExpr is to allow to replace a syntax element by a
    different one (e.g. in SANY, lambda expressions are defined operators which
    are replaced by the actual lambda language construct. The first one is not
    of type expr, but the second one is.). The default implementation just
    duplicates the given tree.
 *)

type 'a macc =  anyExpr * 'a


class ['a] expr_map : object
  method expr            : 'a macc -> expr -> 'a macc
  method name            : 'a macc -> string -> 'a macc
  method location        : 'a macc -> location -> 'a macc
  method level           : 'a macc -> level option -> 'a macc
  method decimal         : 'a macc -> decimal -> 'a macc
  method numeral         : 'a macc -> numeral -> 'a macc
  method strng           : 'a macc -> strng -> 'a macc
  method at              : 'a macc -> at -> 'a macc
  method op_appl         : 'a macc -> op_appl -> 'a macc
  method binder          : 'a macc -> binder -> 'a macc
  method lambda          : 'a macc -> lambda -> 'a macc
  method op_arg          : 'a macc -> op_arg -> 'a macc
  method operator        : 'a macc -> operator -> 'a macc
  method expr_or_op_arg  : 'a macc -> expr_or_op_arg -> 'a macc
  method bound_symbol    : 'a macc -> bound_symbol -> 'a macc
  method bounded_bound_symbol   : 'a macc -> bounded_bound_symbol -> 'a macc
  method unbounded_bound_symbol : 'a macc -> unbounded_bound_symbol -> 'a macc
  method mule            : 'a macc -> mule -> 'a macc
  method formal_param    : 'a macc -> formal_param -> 'a macc
  method op_decl         : 'a macc -> op_decl -> 'a macc
  method op_def          : 'a macc -> op_def -> 'a macc
  method theorem         : 'a macc -> theorem -> 'a macc
  method assume          : 'a macc -> assume -> 'a macc
  method assume_prove    : 'a macc -> assume_prove -> 'a macc
  method new_symb        : 'a macc -> new_symb -> 'a macc
  method ap_subst_in     : 'a macc -> ap_subst_in -> 'a macc
  method module_instance : 'a macc -> module_instance -> 'a macc
  method builtin_op      : 'a macc -> builtin_op -> 'a macc
  method user_defined_op : 'a macc -> user_defined_op -> 'a macc
  method proof           : 'a macc -> proof -> 'a macc
  method step            : 'a macc -> step -> 'a macc
  method instance        : 'a macc -> instance -> 'a macc
  method use_or_hide     : 'a macc -> use_or_hide -> 'a macc
  method subst           : 'a macc -> subst -> 'a macc
  method label           : 'a macc -> label -> 'a macc
  method let_in          : 'a macc -> let_in -> 'a macc
  method subst_in        : 'a macc -> subst_in -> 'a macc
  method node            : 'a macc -> node -> 'a macc
  method def_step        : 'a macc -> def_step -> 'a macc
  method reference       : 'a macc -> int -> 'a macc

  method entry           : 'a macc -> (int * entry) -> 'a macc
  method context         : 'a macc -> context -> 'a macc

  method op_appl_or_binder : 'a macc -> op_appl_or_binder -> 'a macc
  method expr_or_module_or_module_instance :
           'a macc -> expr_or_module_or_module_instance -> 'a macc
  method defined_expr : 'a macc -> defined_expr -> 'a macc
  method op_def_or_theorem_or_assume       :
           'a macc -> op_def_or_theorem_or_assume -> 'a macc

end
