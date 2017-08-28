open Commons
open Expr_ds
open Expr_builtins

module Constr : sig
  val fp : term_db:term_db -> location:location -> level:level option -> string -> int
    -> term_db * formal_param
  (** Constructs a formal parameter *)

  val constant : term_db:term_db -> location:location -> string -> int
    -> term_db * op_decl
  (** Constructs a constant declaration *)

  val variable : term_db:term_db -> location:location -> string -> int
    -> term_db * op_decl
  (** Constructs a variable declaration *)

  val uop_def : term_db:term_db -> location:location -> string -> expr ->
    (formal_param * bool) list -> term_db * user_defined_op
  (** Constructs a non-recursive operator definition (recursiveness unchecked) *)

  val rec_uop_def : term_db:term_db -> location:location -> string -> expr ->
    (formal_param * bool) list -> term_db * user_defined_op
  (** Constructs a recursive operator definition *)

  val neg  : term_db:term_db -> location:location -> expr_or_op_arg
    -> op_appl
  (** Constructs a negation *)
  val conj : term_db:term_db -> location:location -> expr_or_op_arg
    -> expr_or_op_arg -> op_appl
  (** Constructs a conjunction *)
  val disj : term_db:term_db -> location:location -> expr_or_op_arg
    -> expr_or_op_arg -> op_appl
  (** Constructs a disjunction *)
  val impl : term_db:term_db -> location:location -> expr_or_op_arg
    -> expr_or_op_arg -> op_appl
  (** Constructs a implication *)
  val eqality : term_db:term_db -> location:location -> expr_or_op_arg
    -> expr_or_op_arg -> op_appl
  (** Constructs an equation *)
  val nequality : term_db:term_db -> location:location -> expr_or_op_arg
    -> expr_or_op_arg -> op_appl
  (** Constructs a negated equation *)

  val quantifier : term_db:term_db  -> location:location -> level:(level option)
    -> Builtin.builtin_symbol -> bound_symbol list -> expr_or_op_arg -> binder
  (** Creates an unbounded quantifier expression *)

  val bquantifier : term_db:term_db -> location:location -> level:(level option)
    -> Builtin.builtin_symbol -> bound_symbol list -> expr_or_op_arg -> binder
  (** Creates a bounded quantifier expression *)

  val apply : location:location -> level:(level option) -> operator ->
    expr_or_op_arg list -> op_appl
  (** Creates an operator application *)

  (*  val guard_of_binder : term_db -> bounded_bound_symbol -> expr *)
  (** Converts a bounded symbol  *)
end
