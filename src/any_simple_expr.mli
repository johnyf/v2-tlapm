open Commons
open Simple_expr_ds

type anySimpleExpr =
  | Nothing
  | Any_location of location
  | Any_level of level option
  | Any_name of string
  | Any_reference of int
  | Any_expr of simple_expr
  | Any_expr_or_op_arg of simple_expr_or_op_arg
  | Any_assume_prove of simple_assume_prove
  | Any_new_symb of simple_new_symb
  | Any_op_def of simple_op_def
  | Any_user_defined_op of simple_user_defined_op
  | Any_user_defined_op_ of simple_user_defined_op_
  | Any_builtin_op of simple_builtin_op
  | Any_op_arg of simple_op_arg
  | Any_formal_param of simple_formal_param
  | Any_formal_param_ of simple_formal_param_
  | Any_op_decl of simple_op_decl
  | Any_op_decl_ of simple_op_decl_
  | Any_expr_or_module_or_module_instance of simple_expr_or_module_or_module_instance
  | Any_defined_expr of simple_defined_expr
  | Any_at of simple_at
  | Any_decimal of simple_decimal
  | Any_label of simple_label
  | Any_op_def_or_theorem_or_assume of simple_op_def_or_theorem_or_assume
  | Any_let_in of simple_let_in
  | Any_numeral of simple_numeral
  | Any_strng of simple_strng
  | Any_operator of simple_operator
  | Any_op_appl of simple_op_appl
  | Any_op_appl_or_binder of simple_op_appl_or_binder
  | Any_binder of simple_binder
  | Any_lambda of simple_lambda
  | Any_bound_symbol of simple_bound_symbol
  | Any_unbounded_bound_symbol of simple_unbounded_bound_symbol
  | Any_bounded_bound_symbol of simple_bounded_bound_symbol
  | Any_mule_entry of simple_mule_entry
  | Any_entry of (int * simple_entry)

type 'a macc =  anySimpleExpr * 'a

val format_anysimple : anySimpleExpr -> string
