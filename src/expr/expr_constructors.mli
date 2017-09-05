open Commons
open Expr_ds
open Expr_builtins

module Constr : sig
  val numeral : location:location -> int -> expr
  (** Constructs a numeral expression *)
  val decimal : location:location -> int -> int -> expr
  (** Constructs a decimal expression *)
  val strng   : location:location -> string -> expr
  (** Constructs a string expression *)

  val fp : term_db:term_db -> location:location -> level:level option -> string
    -> int -> term_db * formal_param
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

  val conjs : term_db:term_db -> location:location -> expr list -> expr
  (** Constructs a right-associative series of conjunctions *)
  val disjs : term_db:term_db -> location:location -> expr list -> expr
  (** Constructs a right-associative series of disjunctions *)
  val impls : term_db:term_db -> location:location -> expr list -> expr
  (** Constructs a right-associative series of implications *)

  val quantifier : term_db:term_db  -> location:location -> level:(level option)
    -> Builtin.builtin_symbol -> bound_symbol list -> expr_or_op_arg -> binder
  (** Creates an unbounded quantifier expression *)

  val bquantifier : term_db:term_db -> location:location -> level:(level option)
    -> Builtin.builtin_symbol -> bound_symbol list -> expr_or_op_arg -> binder
  (** Creates a bounded quantifier expression *)

  val apply : term_db:term_db -> location:location -> level:(level option) ->
    operator -> expr_or_op_arg list -> op_appl
  (** Creates an operator application *)

  (*  val guard_of_binder : term_db -> bounded_bound_symbol -> expr *)
  (** Converts a bounded symbol  *)

  module OPD : sig
    val mi      : module_instance -> op_def
    val uop     : user_defined_op -> op_def
    val bop     : builtin_op -> op_def
    val thm_def : theorem_def -> op_def
    val assume_def : assume_def -> op_def

    val location_of : term_db -> op_def -> location
    val level_of    : term_db -> op_def -> level option
    val arity_of    : term_db -> op_def -> int
  end

  module Op : sig
    val formal_param : formal_param -> operator
    val op_def       : op_def -> operator
    val op_decl      : op_decl -> operator
    val ap_subst_in  : ap_subst_in -> operator
    val lamda        : lambda -> operator

    val location_of : term_db -> operator -> location
    val level_of    : term_db -> operator -> level option
    val arity_of    : term_db -> operator -> int
  end

  module E : sig
    val at : at -> expr
    val decimal : decimal -> expr
    val label : label -> expr
    val let_in : let_in -> expr
    val numeral : numeral -> expr
    val op_appl : op_appl -> expr
    val strng : strng -> expr
    val subst_in : subst_in -> expr
    val fp_subst_in : fp_subst_in -> expr
    val binder : binder -> expr
  end

  module EO : sig
    val expr   : expr -> expr_or_op_arg
    val op_arg : op_arg -> expr_or_op_arg
  end

end
