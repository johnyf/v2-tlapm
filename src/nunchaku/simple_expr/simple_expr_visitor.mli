open Commons
open Simple_expr_ds

(**
   A visitor pattern for data-structures from the {!module:Expr_ds} module.

   Each data-type has a corresponding method of the same name. A special
   visitor for the name field is provided. The default implementation passes
   the accumulator down unchanged, the only property which is not explicitly
   handled is the arity of operators.
*)

class ['a] visitor :
  object
    method expr            : 'a -> simple_expr -> 'a
    method name            : 'a -> string -> 'a
    method location        : 'a -> location -> 'a
    method level           : 'a -> level option -> 'a
    method decimal         : 'a -> simple_decimal -> 'a
    method numeral         : 'a -> simple_numeral -> 'a
    method strng           : 'a -> simple_strng -> 'a
    method at              : 'a -> simple_at -> 'a
    method op_appl         : 'a -> simple_op_appl -> 'a
    method binder          : 'a -> simple_binder -> 'a
    method lambda          : 'a -> simple_lambda -> 'a
    method op_arg          : 'a -> simple_op_arg -> 'a
    method operator        : 'a -> simple_operator -> 'a
    method expr_or_op_arg  : 'a -> simple_expr_or_op_arg -> 'a
    method bound_symbol    : 'a -> simple_bound_symbol -> 'a
    method bounded_bound_symbol   : 'a -> simple_bounded_bound_symbol -> 'a
    method unbounded_bound_symbol : 'a -> simple_unbounded_bound_symbol -> 'a
    method formal_param    : 'a -> simple_formal_param -> 'a
    method op_decl         : 'a -> simple_op_decl -> 'a
    method op_def          : 'a -> simple_op_def -> 'a
    method assume_prove    : 'a -> simple_assume_prove -> 'a
    method new_symb        : 'a -> simple_new_symb -> 'a
    method builtin_op      : 'a -> simple_builtin_op -> 'a
    method user_defined_op : 'a -> simple_user_defined_op -> 'a
    method label           : 'a -> simple_label -> 'a
    method let_in          : 'a -> simple_let_in -> 'a
    method reference       : 'a -> int -> 'a

    method entry           : 'a -> (int * simple_entry) -> 'a

    method op_appl_or_binder : 'a -> simple_op_appl_or_binder -> 'a
    method expr_or_module_or_module_instance
      : 'a -> simple_expr_or_module_or_module_instance -> 'a
    method defined_expr
      : 'a -> simple_defined_expr -> 'a
    method op_def_or_theorem_or_assume
      : 'a -> simple_op_def_or_theorem_or_assume -> 'a

  end
