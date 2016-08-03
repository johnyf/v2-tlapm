open Expr_ds
open Commons

type nesting = Module | Expression | ProofStep of int | By
type fc = Format.formatter * term_db * bool * nesting * int

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
    method binder          : fc -> binder -> fc
    method lambda          : fc -> lambda -> fc
    method op_arg          : fc -> op_arg -> fc
    method operator        : fc -> operator -> fc
    method expr_or_op_arg  : fc -> expr_or_op_arg -> fc
    method bound_symbol    : fc -> bound_symbol -> fc
    method bounded_bound_symbol   : fc -> bounded_bound_symbol -> fc
    method unbounded_bound_symbol : fc -> unbounded_bound_symbol -> fc
    method mule            : fc -> mule -> fc
    method mule_entry      : fc -> mule_entry -> fc
    method formal_param    : fc -> formal_param -> fc
    method op_decl         : fc -> op_decl -> fc
    method op_def          : fc -> op_def -> fc
    method theorem         : fc -> theorem -> fc
    method statement       : fc -> statement -> fc
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
    method instantiation   : fc -> instantiation -> fc
    (*  method subst           : fc -> subst -> fc *)
    method label           : fc -> label -> fc
    method let_in          : fc -> let_in -> fc
    method subst_in        : fc -> subst_in -> fc
    method node            : fc -> node -> fc
    method def_step        : fc -> def_step -> fc
    method reference       : fc -> int -> fc

    method entry           : fc -> (int * entry) -> fc
    method context         : fc -> context -> fc

    method op_appl_or_binder : fc -> op_appl_or_binder -> fc
    method expr_or_module_or_module_instance :
      fc -> expr_or_module_or_module_instance -> fc
    method defined_expr : fc -> defined_expr -> fc
    method op_def_or_theorem_or_assume       :
      fc -> op_def_or_theorem_or_assume -> fc

    method translate_builtin_name : string -> string

  end

(** An instance of the expression formatter *)
val expr_formatter : formatter

(** Creates a function which is compatible with printf's %a statement. *)
val mk_fmt           : (fc -> 'a -> fc) ->
  term_db -> out_channel -> 'a -> unit


val fmt_expr         : term_db -> out_channel -> expr -> unit
val fmt_assume_prove : term_db -> out_channel -> assume_prove -> unit
val fmt_statement    : term_db -> out_channel -> statement -> unit
