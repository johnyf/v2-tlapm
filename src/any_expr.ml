open Commons
open Expr_ds

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
  | Any_assume_prove of assume_prove
  | Any_new_symb of new_symb
  | Any_op_def of op_def
  | Any_op_def_ of op_def_
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
  | Any_context of context
  | Any_entry of (int * entry)
     
type 'a macc =  anyExpr * 'a

class ['a] any_extractor = object(self)
  method extract: ('a -> anyExpr) = fun x -> Nothing

  method ap_subst_in acc =
    match self#extract acc with Any_ap_subst_in x -> x
  method assume acc =
    match self#extract acc with Any_assume x -> x
  method assume_prove acc =
    match self#extract acc with Any_assume_prove x -> x
  method at acc =
    match self#extract acc with Any_at x -> x
  method bounded_bound_symbol acc =
    match self#extract acc with Any_bounded_bound_symbol x -> x
  method builtin_op acc =
    match self#extract acc with Any_builtin_op x -> x
  method decimal acc =
    match self#extract acc with Any_decimal x -> x
  method def_step acc =
    match self#extract acc with Any_def_step x -> x
  method expr acc =
    match self#extract acc with Any_expr x -> x
  method expr_or_op_arg acc =
    match self#extract acc with Any_expr_or_op_arg x -> x
  method formal_param acc =
    match self#extract acc with Any_formal_param x -> x
  method instance acc =
    match self#extract acc with Any_instance x -> x
  method label acc =
    match self#extract acc with Any_label x -> x
  method let_in acc =
    match self#extract acc with Any_let_in x -> x
  method level acc =
    match self#extract acc with Any_level x -> x
  method location acc =
    match self#extract acc with Any_location x -> x
  method module_instance acc =
    match self#extract acc with Any_module_instance x -> x
  method mule acc =
    match self#extract acc with Any_mule x -> x
  method node acc =
    match self#extract acc with Any_node x -> x
  method numeral acc =
    match self#extract acc with Any_numeral x -> x
  method op_appl_or_binder acc =
    match self#extract acc with Any_op_appl_or_binder x -> x
  method op_arg acc =
    match self#extract acc with Any_op_arg x -> x
  method op_decl acc =
    match self#extract acc with Any_op_decl x -> x
  method op_def acc =
    match self#extract acc with Any_op_def x -> x
  method operator acc =
    match self#extract acc with Any_operator x -> x
  method proof acc =
    match self#extract acc with Any_proof x -> x
  method strng acc =
    match self#extract acc with Any_strng x -> x
  method subst_in acc =
    match self#extract acc with Any_subst_in x -> x
  method theorem acc =
    match self#extract acc with Any_theorem x -> x
  method unbounded_bound_symbol acc =
    match self#extract acc with Any_unbounded_bound_symbol x -> x
  method use_or_hide acc =
    match self#extract acc with Any_use_or_hide x -> x
  method user_defined_op acc =
    match self#extract acc with Any_user_defined_op x -> x
end
