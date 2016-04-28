open Simple_expr_ds
open Commons
open Format
open Nunchaku_statement
       
type fc = nun_statement * simple_term_db * string
	    
class formatter :
object
  method expr            : fc -> simple_expr -> fc
  method name            : fc -> string -> fc
  method location        : fc -> location -> fc
  method level           : fc -> level option -> fc
  method decimal         : fc -> simple_decimal -> fc
  method numeral         : fc -> simple_numeral -> fc
  method strng           : fc -> simple_strng -> fc
  method at              : fc -> simple_at -> fc
  method op_appl         : fc -> simple_op_appl -> fc
  method binder          : fc -> simple_binder -> fc
  method lambda          : fc -> simple_lambda -> fc
  method op_arg          : fc -> simple_op_arg -> fc
  method operator        : fc -> simple_operator -> fc
  method expr_or_op_arg  : fc -> simple_expr_or_op_arg -> fc
  method bound_symbol    : fc -> simple_bound_symbol -> fc
  method bounded_bound_symbol   : fc -> simple_bounded_bound_symbol -> fc
  method unbounded_bound_symbol : fc -> simple_unbounded_bound_symbol -> fc
  method formal_param    : fc -> simple_formal_param -> fc
  method op_decl         : fc -> simple_op_decl -> fc
  method op_def          : fc -> simple_op_def -> fc
  method assume_prove    : fc -> simple_assume_prove -> fc
  method new_symb        : fc -> simple_new_symb -> fc
  method builtin_op      : fc -> simple_builtin_op -> fc
  method user_defined_op : fc -> simple_user_defined_op -> fc
  method label           : fc -> simple_label -> fc
  method let_in          : fc -> simple_let_in -> fc
  method reference       : fc -> int -> fc

  method entry           : fc -> (int * simple_entry) -> fc

  method op_appl_or_binder : fc -> simple_op_appl_or_binder -> fc
  method expr_or_module_or_module_instance :
           fc -> simple_expr_or_module_or_module_instance -> fc
  method defined_expr : fc -> simple_defined_expr -> fc
  method op_def_or_theorem_or_assume       :
           fc -> simple_op_def_or_theorem_or_assume -> fc

  method translate_builtin_name : string -> string

end

(** An instance of the expression formatter *)
val expr_formatter : formatter

(** Creates a function which is compatible with printf's %a statement. *)
val mk_fmt           : (fc -> 'a -> fc) ->
                       simple_term_db -> 'a -> nun_statement


val fmt_expr         : simple_term_db -> simple_expr -> nun_statement
val fmt_assume_prove : simple_term_db -> simple_assume_prove -> nun_statement
