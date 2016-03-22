open Commons
open Simple_expr_ds

type anySimpleExpr =
  | Nothing
  | Any_location of location
  | Any_level of level option
  | Any_name of string
  | Any_reference of int
  | Any_expr of expr
  | Any_expr_or_op_arg of expr_or_op_arg
  | Any_assume_prove of assume_prove
  | Any_new_symb of new_symb
  | Any_op_def of op_def
  | Any_user_defined_op of user_defined_op
  | Any_user_defined_op_ of user_defined_op_
  | Any_builtin_op of builtin_op
  | Any_op_arg of op_arg
  | Any_formal_param of formal_param
  | Any_formal_param_ of formal_param_
  | Any_op_decl of op_decl
  | Any_op_decl_ of op_decl_
  | Any_expr_or_module_or_module_instance of expr_or_module_or_module_instance
  | Any_defined_expr of defined_expr
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
  | Any_mule_entry of mule_entry
  | Any_entry of (int * entry)

type 'a macc =  anySimpleExpr * 'a

val format_anysimple : anySimpleExpr -> string
