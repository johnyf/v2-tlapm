open Expr_ds
open Commons
open CCFormat

type nesting = Module | Expression | ProofStep of int | By
type fc = Format.formatter * term_db * bool * nesting * int

class formatter :
  object
    inherit [fc] Expr_visitor.visitor
    method translate_builtin_name : string -> string

  end

(** An instance of the expression formatter *)
val expr_formatter : formatter

(** Creates a function which is compatible with printf's %a statement. *)
val mk_printer           : (fc -> 'a -> fc) ->
  term_db -> out_channel -> 'a -> unit


val fmt_expr           : term_db -> expr printer
val fmt_assume_prove   : term_db -> assume_prove printer
val fmt_node           : term_db -> node printer
val fmt_statement      : term_db -> statement printer
val fmt_formal_param   : term_db -> formal_param printer
val fmt_expr_or_op_arg : term_db -> expr_or_op_arg printer
val fmt_op_decl        : term_db -> op_decl printer
val fmt_bound_symbol   : term_db -> bound_symbol printer
val fmt_operator       : term_db -> operator printer

val prnt_expr         : term_db -> out_channel -> expr -> unit
val prnt_assume_prove : term_db -> out_channel -> assume_prove -> unit
val prnt_statement    : term_db -> out_channel -> statement -> unit
