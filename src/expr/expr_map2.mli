open Commons
open Expr_ds
open Expr_visitor
open Any_expr

(** A stricter map than expr_map without using any_expr *)

type ('a, 'b) macc =  'a * 'b

val return  : 'b -> 'a -> ('a, 'b) macc
val opt_map : ('a -> 'b -> 'c * 'a) -> 'a -> 'b option -> 'c option * 'a
val fold    : ('a -> 'b -> 'c * 'a) -> 'a -> 'b list -> 'c list * 'a

class ['a] expr_map : object
  method expr                   : 'a -> expr -> (expr, 'a) macc
  method name                   : 'a -> string -> (string,'a) macc
  method location               : 'a -> location -> (location,'a) macc
  method level                  : 'a -> level -> (level,'a) macc
  method decimal                : 'a -> decimal -> (decimal, 'a) macc
  method numeral                : 'a -> numeral -> (numeral, 'a) macc
  method strng                  : 'a -> strng -> (strng, 'a) macc
  method at                     : 'a -> at -> (at, 'a) macc
  method op_appl                : 'a -> op_appl -> (op_appl, 'a) macc
  method binder                 : 'a -> binder -> (binder, 'a) macc
  method lambda                 : 'a -> lambda -> (lambda, 'a) macc
  method op_arg                 : 'a -> op_arg -> (op_arg, 'a) macc
  method operator               : 'a -> operator -> (operator, 'a) macc
  method expr_or_op_arg         : 'a -> expr_or_op_arg -> (expr_or_op_arg, 'a) macc
  method bound_symbol           : 'a -> bound_symbol -> (bound_symbol, 'a) macc
  method bounded_bound_symbol   : 'a -> bounded_bound_symbol -> (bounded_bound_symbol, 'a) macc
  method unbounded_bound_symbol : 'a -> unbounded_bound_symbol -> (unbounded_bound_symbol, 'a) macc
  method mule                   : 'a -> mule -> (mule, 'a) macc
  method mule_                  : 'a -> mule_ -> (mule_, 'a) macc
  method mule_entry             : 'a -> mule_entry -> (mule_entry, 'a) macc
  method formal_param           : 'a -> formal_param -> (formal_param, 'a) macc
  method formal_param_          : 'a -> formal_param_ -> (formal_param_, 'a) macc
  method fp_subst_in            : 'a -> fp_subst_in -> (fp_subst_in, 'a) macc
  method fp_assignment          : 'a -> fp_assignment -> (fp_assignment, 'a) macc
  method op_decl                : 'a -> op_decl -> (op_decl, 'a) macc
  method op_decl_               : 'a -> op_decl_ -> (op_decl_, 'a) macc
  method op_def                 : 'a -> op_def -> (op_def, 'a) macc
  method theorem                : 'a -> theorem -> (theorem, 'a) macc
  method theorem_               : 'a -> theorem_ -> (theorem_, 'a) macc
  method theorem_def            : 'a -> theorem_def -> (theorem_def, 'a) macc
  method theorem_def_           : 'a -> theorem_def_ -> (theorem_def_, 'a) macc
  method statement              : 'a -> statement -> (statement, 'a) macc
  method assume                 : 'a -> assume -> (assume, 'a) macc
  method assume_                : 'a -> assume_ -> (assume_, 'a) macc
  method assume_def             : 'a -> assume_def -> (assume_def, 'a) macc
  method assume_def_            : 'a -> assume_def_ -> (assume_def_, 'a) macc
  method assume_prove           : 'a -> assume_prove -> (assume_prove, 'a) macc
  method new_symb               : 'a -> new_symb -> (new_symb, 'a) macc
  method ap_subst_in            : 'a -> ap_subst_in -> (ap_subst_in, 'a) macc
  method module_instance        : 'a -> module_instance -> (module_instance, 'a) macc
  method module_instance_       : 'a -> module_instance_ -> (module_instance_, 'a) macc
  method builtin_op             : 'a -> builtin_op -> (builtin_op, 'a) macc
  method builtin_op_            : 'a -> builtin_op_ -> (builtin_op_, 'a) macc
  method user_defined_op        : 'a -> user_defined_op -> (user_defined_op, 'a) macc
  method user_defined_op_       : 'a -> user_defined_op_ -> (user_defined_op_, 'a) macc
  method proof                  : 'a -> proof -> (proof, 'a) macc
  method step                   : 'a -> step -> (step, 'a) macc
  method instance               : 'a -> instance -> (instance, 'a) macc
  method use_or_hide            : 'a -> use_or_hide -> (use_or_hide, 'a) macc
  method instantiation          : 'a -> instantiation -> (instantiation, 'a) macc
  method label                  : 'a -> label -> (label, 'a) macc
  method let_in                 : 'a -> let_in -> (let_in, 'a) macc
  method subst_in               : 'a -> subst_in -> (subst_in, 'a) macc
  method node                   : 'a -> node -> (node, 'a) macc
  method def_step               : 'a -> def_step -> (def_step, 'a) macc
  method reference              : 'a -> int -> (int, 'a) macc
  method entry                  : 'a -> (int * entry) -> ((int * entry), 'a) macc
  method context                : 'a -> context -> (context, 'a) macc

  method op_appl_or_binder :
    'a -> op_appl_or_binder -> (op_appl_or_binder, 'a) macc
  method expr_or_module_or_module_instance :
    'a -> expr_or_module_or_module_instance -> (expr_or_module_or_module_instance, 'a) macc
  method defined_expr :
    'a -> defined_expr -> (defined_expr, 'a) macc
  method op_def_or_theorem_or_assume       :
    'a -> op_def_or_theorem_or_assume -> (op_def_or_theorem_or_assume,'a) macc

end
