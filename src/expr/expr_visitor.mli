open Commons
open Expr_ds

(**
   A visitor pattern for data-structures from the {!module:Expr_ds} module.

   Each data-type has a corresponding method of the same name. A special
   visitor for the name field is provided. The default implementation passes
   the accumulator down unchanged, the only property which is not explicitly
   handled is the arity of operators.
*)

class ['a] visitor :
  object
    method expr            : 'a -> expr -> 'a
    method name            : 'a -> string -> 'a
    method location        : 'a -> location -> 'a
    method level           : 'a -> level option -> 'a
    method decimal         : 'a -> decimal -> 'a
    method numeral         : 'a -> numeral -> 'a
    method strng           : 'a -> strng -> 'a
    method at              : 'a -> at -> 'a
    method op_appl         : 'a -> op_appl -> 'a
    method binder          : 'a -> binder -> 'a
    method lambda          : 'a -> lambda -> 'a
    method op_arg          : 'a -> op_arg -> 'a
    method operator        : 'a -> operator -> 'a
    method expr_or_op_arg  : 'a -> expr_or_op_arg -> 'a
    method bound_symbol    : 'a -> bound_symbol -> 'a
    method bounded_bound_symbol   : 'a -> bounded_bound_symbol -> 'a
    method unbounded_bound_symbol : 'a -> unbounded_bound_symbol -> 'a
    method mule            : 'a -> mule -> 'a
    method mule_entry      : 'a -> mule_entry -> 'a
    method formal_param    : 'a -> formal_param -> 'a
    method op_decl         : 'a -> op_decl -> 'a
    method op_def          : 'a -> op_def -> 'a
    method theorem         : 'a -> theorem -> 'a
    method statement       : 'a -> statement -> 'a
    method assume          : 'a -> assume -> 'a
    method assume_prove    : 'a -> assume_prove -> 'a
    method new_symb        : 'a -> new_symb -> 'a
    method ap_subst_in     : 'a -> ap_subst_in -> 'a
    method module_instance : 'a -> module_instance -> 'a
    method builtin_op      : 'a -> builtin_op -> 'a
    method user_defined_op : 'a -> user_defined_op -> 'a
    method proof           : 'a -> proof -> 'a
    method step            : 'a -> step -> 'a
    method instance        : 'a -> instance -> 'a
    method use_or_hide     : 'a -> use_or_hide -> 'a
    method instantiation   : 'a -> instantiation -> 'a
    (*  method subst           : 'a -> subst -> 'a *)
    method label           : 'a -> label -> 'a
    method let_in          : 'a -> let_in -> 'a
    method subst_in        : 'a -> subst_in -> 'a
    method node            : 'a -> node -> 'a
    method def_step        : 'a -> def_step -> 'a
    method reference       : 'a -> int -> 'a

    method formal_param_   : 'a -> formal_param_ -> 'a
    method mule_           : 'a -> mule_ -> 'a
    method op_decl_        : 'a -> op_decl_ -> 'a
    method module_instance_  : 'a -> module_instance_ -> 'a
    method user_defined_op_  : 'a -> user_defined_op_ -> 'a
    method builtin_op_     : 'a -> builtin_op_ -> 'a
    method theorem_def_    : 'a -> theorem_def_ -> 'a
    method assume_def_     : 'a -> assume_def_ -> 'a
    method theorem_        : 'a -> theorem_ -> 'a
    method assume_         : 'a -> assume_ -> 'a

    
    method entry           : 'a -> (int * entry) -> 'a
    method context         : 'a -> context -> 'a

    method op_appl_or_binder : 'a -> op_appl_or_binder -> 'a
    method expr_or_module_or_module_instance
      : 'a -> expr_or_module_or_module_instance -> 'a
    method defined_expr
      : 'a -> defined_expr -> 'a
    method op_def_or_theorem_or_assume
      : 'a -> op_def_or_theorem_or_assume -> 'a

  end
