open Commons
open Expr_ds

(** A type holding all possible types in expr_ds, including a formatter and an
    extractor class. Sometimes we do not want to restrict the return value of a
    function to a specific sub-expression in expr_ds. The extractor class
    retrieves the specified element from an any_expr. *)

type anyExpr =
  | Nothing
  | Any_location of location
  | Any_level of level option
  | Any_name of string
  | Any_reference of int
  | Any_node of node
  | Any_expr of expr
  | Any_expr_or_op_arg of expr_or_op_arg
  | Any_ap_subst_in of ap_subst_in
  | Any_subst_in of subst_in
  | Any_instance of instance
  | Any_subst of subst
  | Any_assume of assume
  | Any_assume_ of assume_
  | Any_theorem of theorem
  | Any_theorem_ of theorem_
  | Any_statement of statement
  | Any_assume_prove of assume_prove
  | Any_new_symb of new_symb
  | Any_op_def of op_def
  | Any_module_instance of module_instance
  | Any_module_instance_ of module_instance_
  | Any_user_defined_op of user_defined_op
  | Any_user_defined_op_ of user_defined_op_
  | Any_builtin_op of builtin_op
  | Any_op_arg of op_arg
  | Any_formal_param of formal_param
  | Any_formal_param_ of formal_param_
  | Any_op_decl of op_decl
  | Any_op_decl_ of op_decl_
  | Any_proof of proof
  | Any_omitted of omitted
  | Any_obvious of obvious
  | Any_expr_or_module_or_module_instance of expr_or_module_or_module_instance
  | Any_defined_expr of defined_expr
  | Any_by of by
  | Any_steps of steps
  | Any_step of step
  | Any_def_step of  def_step
  | Any_use_or_hide of use_or_hide
  | Any_at of at
  | Any_decimal of decimal
  | Any_label of label
  | Any_op_def_or_theorem_or_assume of op_def_or_theorem_or_assume
  | Any_let_in of let_in
  | Any_numeral of numeral
  | Any_strng of strng
  | Any_operator of operator
  | Any_op_appl of op_appl
  | Any_op_appl_or_binder of op_appl_or_binder
  | Any_binder of binder
  | Any_lambda of lambda
  | Any_bound_symbol of bound_symbol
  | Any_unbounded_bound_symbol of unbounded_bound_symbol
  | Any_bounded_bound_symbol of bounded_bound_symbol
  | Any_mule of mule
  | Any_mule_ of  mule_
  | Any_mule_entry of mule_entry
  | Any_context of context
  | Any_entry of (int * entry)

val format_anyexpr : anyExpr -> string


class ['a] any_extractor : object
  (** All other any_extractor methods handle the conversion from anyExpr to one
      of the corresponding type. Extract converts from 'a to anyExpr. The
      default implementation maps to Nothing, any useful derived class needs to
      override extract.
  *)
  method extract : 'a -> anyExpr

  method ap_subst_in : 'a -> ap_subst_in
  method assume : 'a -> assume
  method assume_prove : 'a -> assume_prove
  method at : 'a -> at
  method binder : 'a -> binder
  method bound_symbol : 'a -> bound_symbol
  method bounded_bound_symbol : 'a -> bounded_bound_symbol
  method builtin_op : 'a -> builtin_op
  method context : 'a -> context
  method decimal : 'a -> decimal
  method def_step : 'a -> def_step
  method defined_expr : 'a -> defined_expr
  method entry : 'a -> (int * entry)
  method expr : 'a -> expr
  method expr_or_op_arg : 'a -> expr_or_op_arg
  method expr_or_module_or_module_instance : 'a -> expr_or_module_or_module_instance
  method formal_param : 'a -> formal_param
  method instance : 'a -> instance
  method label : 'a -> label
  method lambda : 'a -> lambda
  method let_in : 'a -> let_in
  method level : 'a -> level option
  method location : 'a -> location
  method module_instance : 'a -> module_instance
  method mule : 'a -> mule
  method mule_entry : 'a -> mule_entry
  method name : 'a -> string
  method new_symb : 'a -> new_symb
  method node : 'a -> node
  method numeral : 'a -> numeral
  method op_appl : 'a -> op_appl
  method op_appl_or_binder : 'a -> op_appl_or_binder
  method op_arg : 'a -> op_arg
  method op_decl : 'a -> op_decl
  method op_def : 'a -> op_def
  method op_def_or_theorem_or_assume : 'a -> op_def_or_theorem_or_assume
  method operator : 'a -> operator
  method proof : 'a -> proof
  method reference : 'a -> int
  method statement : 'a -> statement
  method step : 'a -> step
  method strng : 'a -> strng
  method subst_in : 'a -> subst_in
  method subst : 'a -> subst
  method theorem : 'a -> theorem
  method unbounded_bound_symbol : 'a -> unbounded_bound_symbol
  method use_or_hide : 'a -> use_or_hide
  method user_defined_op : 'a -> user_defined_op
end
