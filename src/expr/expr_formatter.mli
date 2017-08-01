open Expr_ds
open Commons

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


val fmt_expr           : term_db -> Format.formatter -> expr -> unit
val fmt_assume_prove   : term_db -> Format.formatter -> assume_prove -> unit
val fmt_node           : term_db -> Format.formatter -> node -> unit
val fmt_statement      : term_db -> Format.formatter -> statement -> unit
val fmt_formal_param   : term_db -> Format.formatter -> formal_param -> unit
val fmt_expr_or_op_arg : term_db -> Format.formatter -> expr_or_op_arg -> unit
val fmt_op_decl        : term_db -> Format.formatter -> op_decl -> unit

val prnt_expr         : term_db -> out_channel -> expr -> unit
val prnt_assume_prove : term_db -> out_channel -> assume_prove -> unit
val prnt_statement    : term_db -> out_channel -> statement -> unit
