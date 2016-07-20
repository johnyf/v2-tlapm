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
  | Any_entry of (int * simple_entry)

type 'a macc =  anySimpleExpr * 'a

val format_anysimple : anySimpleExpr -> string

					  
class ['a] any_extractor : object
  (** All other any_extractor methods handle the conversion from anyExpr to one
      of the corresponding type. Extract converts from 'a to anyExpr. The
      default implementation maps to Nothing, any useful derived class needs to
      override extract.
  *)
  method extract : 'a -> anySimpleExpr
  method private fmt : 'a -> string
  method assume_prove : 'a -> simple_assume_prove
  method at : 'a -> simple_at
  method binder : 'a -> simple_binder
  method bound_symbol : 'a -> simple_bound_symbol
  method bounded_bound_symbol : 'a -> simple_bounded_bound_symbol
  method builtin_op : 'a -> simple_builtin_op
  method decimal : 'a -> simple_decimal
  method defined_expr : 'a -> simple_defined_expr
  method expr : 'a -> simple_expr
  method expr_or_op_arg : 'a -> simple_expr_or_op_arg
  method expr_or_module_or_module_instance : 'a -> simple_expr_or_module_or_module_instance
  method formal_param : 'a -> simple_formal_param
  method label : 'a -> simple_label
  method lambda : 'a -> simple_lambda
  method let_in : 'a -> simple_let_in
  method new_symb : 'a -> simple_new_symb
  method numeral : 'a -> simple_numeral
  method op_appl : 'a -> simple_op_appl
  method op_appl_or_binder : 'a -> simple_op_appl_or_binder
  method op_arg : 'a -> simple_op_arg
  method op_decl : 'a -> simple_op_decl
  method op_def : 'a -> simple_op_def
  method op_def_or_theorem_or_assume : 'a -> simple_op_def_or_theorem_or_assume
  method operator : 'a -> simple_operator
  method reference : 'a -> int
  method strng : 'a -> simple_strng
  method unbounded_bound_symbol : 'a -> simple_unbounded_bound_symbol
  method user_defined_op : 'a -> simple_user_defined_op
end

					 
