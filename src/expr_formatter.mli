open Expr_ds
open Commons

type nesting = Module | Expression | ProofStep
type fc = Format.formatter * context * bool * nesting * int

class formatter :
object
  method expr            : fc -> expr -> fc
  method name            : fc -> string -> fc
  method location        : fc -> location -> fc
  method level           : fc -> level option -> fc
  method decimal         : fc -> decimal -> fc
  method numeral         : fc -> numeral -> fc
  method strng           : fc -> strng -> fc
  method at              : fc -> at -> fc
  method op_appl         : fc -> op_appl -> fc
  method op_arg          : fc -> op_arg -> fc
  method operator        : fc -> operator -> fc
  method expr_or_op_arg  : fc -> expr_or_op_arg -> fc
  method bound_symbol    : fc -> bound_symbol -> fc
  method bounded_bound_symbol   : fc -> bounded_bound_symbol -> fc
  method unbounded_bound_symbol : fc -> unbounded_bound_symbol -> fc
  method mule            : fc -> mule -> fc
  method formal_param    : fc -> formal_param -> fc
  method op_decl         : fc -> op_decl -> fc
  method op_def          : fc -> op_def -> fc
  method theorem         : fc -> theorem -> fc
  method assume          : fc -> assume -> fc
  method assume_prove    : fc -> assume_prove -> fc
  method new_symb        : fc -> new_symb -> fc
  method ap_subst_in     : fc -> ap_subst_in -> fc
  method module_instance : fc -> module_instance -> fc
  method builtin_op      : fc -> builtin_op -> fc
  method user_defined_op : fc -> user_defined_op -> fc
  method proof           : fc -> proof -> fc
  method step            : fc -> step -> fc
  method instance        : fc -> instance -> fc
  method use_or_hide     : fc -> use_or_hide -> fc
  method subst           : fc -> subst -> fc
  method label           : fc -> label -> fc
  method let_in          : fc -> let_in -> fc
  method subst_in        : fc -> subst_in -> fc
  method node            : fc -> node -> fc
  method def_step        : fc -> def_step -> fc
  method reference       : fc -> int -> fc

  method entry           : fc -> (int * entry) -> fc
  method context         : fc -> context -> fc

  method expr_or_module_or_module_instance :
           fc -> expr_or_module_or_module_instance -> fc
  method defined_expr : fc -> defined_expr -> fc
  method op_def_or_theorem_or_assume       :
           fc -> op_def_or_theorem_or_assume -> fc

  method translate_builtin_name : string -> string

end

val expr_formatter : formatter
