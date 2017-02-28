open Commons

(** This datastructurs is similar to expressions but omitting some parts 
    especially the proofs.
*)

(** Represents a TLA expression.
    Example:
    f(1 + "sde")
*)
type simple_expr =
  | E_at of simple_at (* TODO: remove in preprocessing (see @ steps in TLA2-guide) *) 
  | E_decimal of simple_decimal
  | E_label of simple_label (* TODO: remove in (see labeling subexpressions in TLA2-guide *)
  | E_let_in of simple_let_in (* TODO: remove in preprocessing *)
  | E_numeral of simple_numeral
  | E_op_appl of simple_op_appl
  | E_string of simple_strng
  | E_binder of simple_binder
  | E_lambda of simple_lambda

(** The union of expressions and operator arguments.
    Used by substitutions and as operand in applications. *)
and simple_expr_or_op_arg =
  | EO_expr of simple_expr
  | EO_op_arg of simple_op_arg



(** The ASSUME ... PROVE statement of TLA.
    The list new_symbols contains all symbols introduced by NEW
    in the assumptions. Assumptions may be assume-proves themselves.
    The suffices flag signals if the SUFFICES keyword is present,
    changing the order of the goals. The boxed flag corresponds to the
    boxed judgement from the coalescing paper, i.e. if all assumptions
    are of the form [] A. (TODO: check if boxed is correctly described)

    Example:
    ASSUME
    NEW CONSTANT P(_),
    NEW c,
    P(c)
    PROVE
    \E x : P(x)
*)
and simple_assume_prove = {
  location          : location;
  level             : level option;
  new_symbols       : simple_new_symb list;
  assumes           : simple_assume_prove list;
  prove             : simple_expr;
}

(** The NEW keyword of TLA, optionally with the containing set.
    Example: NEW STATE s \in States
*)
and simple_new_symb = {
  location          : location;
  level             : level option;
  op_decl           : simple_op_decl;
  set               : simple_expr option;
}


(** An operator definition, either by instantiating a module,
    a user defined operator or a builtin operator.
*)
and simple_op_def =
  | O_user_defined_op of simple_user_defined_op
  | O_builtin_op of simple_builtin_op


(** A user defined operator or a reference to it.
*)
and simple_user_defined_op =
  | UOP_ref of int (*make it int * string, but makes conversion more complex *)
  | UOP of simple_user_defined_op_

(** A user defined operator in TLA.
    Example:
    Op(x,y) == ENABLED (x' # y')
*)
and simple_user_defined_op_ = {
  location          : location;
  level             : level option;
  name              : string;
  arity             : int;
  body              : simple_expr;
  params            : simple_formal_param list;
  recursive         : bool;
}

(** A lambda abstraction. The difference to binders is that it works on formal
    parameters, not variables.
*)
and simple_lambda = {
  (*TODO: check if a lambda abstraction has a level*)
  location          : location;
  level             : level option;
  arity             : int;
  body              : simple_expr;
  params            : (simple_formal_param * bool (*is leibniz*)) list;
}

(** A builtin operator of TLA. See the TLA book p. 268ff. for a list.
    Each operator is a constant.
    Example: =>, TRUE
*)
and simple_builtin_op = {
  level             : level option;
  name              : string;
  arity             : int;
  params            : (simple_formal_param * bool (*is leibniz*)) list
}

(** An argument for an operator. Apparently the arity is always >0.
    TODO: check what the difference to a formal parameter is.
*)
and simple_op_arg = {
  location          : location;
  level             : level option;
  (*  name              : string; *)
  (* arity             : int; *)
  argument          : simple_operator;
}

(** A formal parameter or a reference to it.*)
and simple_formal_param =
  | FP_ref of int
  | FP of simple_formal_param_

(** Represents arguments of a declared operator.
    Example: x and simple_y in
    Op(x,y) = x + y
*)
and simple_formal_param_ = {
  location          : location;
  level             : level option; (* \A x : x = x' is provable because of the level of x. make sure the level is checked *)
  name              : string;
  arity             : int
}

(** An operator declaration or a reference to it. *)
and simple_op_decl =
  | OPD_ref of int
  | OPD of simple_op_decl_

(** An operator declaration, including operators introduced by NEW.
    Example:
    VARIABLES x,y
    ASSUME NEW P(_), NEW VARIABLE x PROVE P(x)
*)
and simple_op_decl_ = {
  location          : location;
  level             : level option;
  name              : string;
  arity             : int;
  kind              : op_decl_kind
}



(** The union of expressions, modules and module instances. *)
and simple_expr_or_module_or_module_instance = (* TODO: remove *)
  | EMM_expr of simple_expr


(** An defintion which can be expanded. Either a user defined operator,
    a module instance, a theorem or an assumption.
*)
and simple_defined_expr =
  | UMTA_user_defined_op of simple_user_defined_op


and simple_op_appl_or_binder =
  | OB_op_appl of simple_op_appl
  | OB_binder of simple_binder

and simple_at = {
  location          : location;
  level             : level option;
  except            : simple_op_appl_or_binder;
  except_component  : simple_op_appl_or_binder
}

and simple_decimal = {
  location          : location;
  level             : level option;
  mantissa          : int;
  exponent          : int
}

and simple_label = {
  location          : location;
  level             : level option;
  name              : string;
  arity             : int;
  body              : simple_assume_prove;
  params            : simple_formal_param list
}

and simple_op_def_or_theorem_or_assume = (* TODO remove *)
  | OTA_op_def of simple_op_def

(** The LET statement of TLA.
    Example:
    LET T == A /\ B IN T => T
*)
and simple_let_in = {
  location          : location;
  level             : level option;
  body              : simple_expr;
  op_defs           : simple_op_def_or_theorem_or_assume list
}

and simple_numeral = {
  location          : location;
  level             : level option;
  value             : int
}

and simple_strng = {
  location          : location;
  level             : level option;
  value             : string
}

(**
   Operator corresponds to {!module:Sany_ds}'s disjunction type
   formal_param_or_module_or_op_decl_or_op_def_or_theorem_or_assume_or_apsubst.
   An operator is anything which can have arguments applied.
*)

and simple_operator =
  | FMOTA_formal_param of simple_formal_param
  | FMOTA_op_decl of simple_op_decl
  | FMOTA_op_def of simple_op_def

and simple_op_appl = {
  location          : location;
  level             : level option;
  operator          : simple_operator;
  operands          : simple_expr_or_op_arg list;
}

(**
   A binder represents a term (B x: F[x]) where B is one of
   \A,\E,\AA,\EE and x is a bound variable. It can be used wherever
   an application is possible.
*)
and simple_binder = {
  location          : location;
  level             : level option;
  operator          : simple_operator;
  (* TODO:  check if we really only need one operand *)
  operand           : simple_expr_or_op_arg;
  bound_symbols     : simple_bound_symbol list
}


and simple_bound_symbol =
  | B_unbounded_bound_symbol of simple_unbounded_bound_symbol
  | B_bounded_bound_symbol of simple_bounded_bound_symbol

and simple_unbounded_bound_symbol = {
  param             : simple_formal_param;
  tuple             : bool
}

and simple_bounded_bound_symbol = {
  params            : simple_formal_param list;
  tuple             : bool;
  domain            : simple_expr
}


type simple_entry =
    FP_entry of simple_formal_param_ |
    OPDec_entry of simple_op_decl_ |
    OPDef_entry of simple_op_def


type simple_term_db = (int * simple_entry) list
