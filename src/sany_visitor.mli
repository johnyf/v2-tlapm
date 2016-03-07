open Commons
open Sany_ds

(** The visitor class for traversing a tla specification. Traversal usually
    starts at the context node, module (mule) node or the expression (expr)
    node.

    Methods have the same name as the Sany_ds type they are handling, apart from
    fmota, which is just too long to write that way. The name property has
    its own method, but the default implementation skips arities and boolean
    flags within a record. References are treated implicitly within the
    method which handles the referred type. The reference method is only called
    on the id.

    The disjunction types which only delegate to the contained types are
    at the bottom of the file. In most cases, they do not need an override.
*)
class ['a] visitor :
object
  method expr            : 'a -> expr -> 'a
  method name            : 'a -> string -> 'a
  method location        : 'a -> location option -> 'a
  method level           : 'a -> level option -> 'a
  method decimal         : 'a -> decimal -> 'a
  method numeral         : 'a -> numeral -> 'a
  method strng           : 'a -> strng -> 'a
  method at              : 'a -> at -> 'a
  method op_appl         : 'a -> op_appl -> 'a
  method op_arg          : 'a -> op_arg -> 'a
  method bound_symbol    : 'a -> bound_symbol -> 'a
  method bounded_bound_symbol   : 'a -> bounded_bound_symbol -> 'a
  method unbounded_bound_symbol : 'a -> unbounded_bound_symbol -> 'a
  method mule            : 'a -> mule -> 'a
  method mule_entry      : 'a -> mule_entry -> 'a
  method formal_param    : 'a -> formal_param -> 'a
  method op_decl         : 'a -> op_decl -> 'a
  method op_def          : 'a -> op_def -> 'a
  method theorem         : 'a -> theorem -> 'a
  method assume          : 'a -> assume -> 'a
  method assume_prove    : 'a -> assume_prove -> 'a
  method new_symb        : 'a -> new_symb -> 'a
  method ap_subst_in     : 'a -> ap_subst_in -> 'a
  method module_instance : 'a -> module_instance -> 'a
  method builtin_op      : 'a -> builtin_op -> 'a
  method user_defined_op : 'a -> user_defined_op -> 'a
  method proof           : 'a -> proof -> 'a
  method step            : 'a -> step -> 'a
  method use_or_hide     : 'a -> use_or_hide -> 'a
  method instance        : 'a -> instance -> 'a
  method label           : 'a -> label -> 'a
  method let_in          : 'a -> let_in -> 'a
  method subst_in        : 'a -> subst_in -> 'a
  method subst           : 'a -> subst -> 'a
  method node            : 'a -> node -> 'a
  method def_step        : 'a -> def_step -> 'a
  method reference       : 'a -> int -> 'a
  method context         : 'a -> context -> 'a
  method entry           : 'a -> entry -> 'a

  (** handlers for disjunction types *)
  method expr_or_assume_prove : 'a -> expr_or_assume_prove -> 'a
  method expr_or_op_arg       : 'a -> expr_or_op_arg -> 'a
  method fmota                : 'a -> formal_param_or_module_or_op_decl_or_op_def_or_theorem_or_assume_or_apsubst -> 'a
  method expr_or_module_or_module_instance                       : 'a -> expr_or_module_or_module_instance -> 'a
  method user_defined_op_or_module_instance_or_theorem_or_assume : 'a -> user_defined_op_or_module_instance_or_theorem_or_assume -> 'a
  method new_symb_or_expr_or_assume_prove                        : 'a -> new_symb_or_expr_or_assume_prove -> 'a
  method op_def_or_theorem_or_assume                             : 'a -> op_def_or_theorem_or_assume -> 'a
end
