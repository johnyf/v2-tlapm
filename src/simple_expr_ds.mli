open Commons

(**
  These datastructures are close to the xml parser datastructures in sany_ds.
  The differences are:
  {ol
   {- instead of location option we use dummy values (Commons.mkDummyLocation)}
   {- the expr in expr_or_assume_prove is replaced by assume_prove without
     assumptions }
   {- new_symb_or_expr_or_assume_prove is reduced to assume_prove, similar to 2
     the new symbols are stored in a separate list in assume prove. the order
     in which they are declared is only relevant for the sany parser. }
   {- builtin operator references are expanded to applications of $builtinname.
     the expr_visitor will provide callbacks for each builtin.

     builtins don't have a location anymore. }
   {- entries in the context cannot contain references anymore }
   {- module instance references are unfolded. }
   {- operator definition references contain the name }
   {- binders were included in opappl before. now they are their own
      expression type  }
   {- lambda abstractions are explicit now instead of being treated as
      user defined operator with name LAMBDA  }
   {- Renamings:
    formal_param_or_module_or_op_decl_or_op_def_or_theorem_or_assume_or_apsubst = operator
    user_defined_op_or_module_instance_or_theorem_or_assume = defined_expr
   }
  }

  Still open:
  {ul
   {- what to do about references? by default, we don't unfold definitions, etc.
     does it make so much more sense to refer to them by name instead of ints?
     this only works for named objects like definitions, but not for unnamed
     theorems etc.}
   {- Module instance refer to modules by name, but they don't exist in the
     representation anymore.
     At the moment, we try to infer the module name from the definition name. }
  }

  How the datastructures work:
  {ol
    {- The context contains the actual data-structures with an index of a unique
       reference id. Parent structures refer to their nested children using the
       id, also within the context. }
    {- The references may be cyclic (because of recursive definitions) }
  }

 *)

 (** Represents a TLA expression.
    Example:
    f(1 + "sde")
  *)
type expr =
  | E_at of at (* TODO: remove in preprocessing (see @ steps in TLA2-guide) *) 
  | E_decimal of decimal
  | E_label of label (* TODO: remove in (see labeling subexpressions in TLA2-guide *)
  | E_let_in of let_in (* TODO: remove in preprocessing *)
  | E_numeral of numeral
  | E_op_appl of op_appl
  | E_string of strng
  | E_binder of binder
  | E_lambda of lambda

 (** The union of expressions and operator arguments.
    Used by substitutions and as operand in applications. *)
 and expr_or_op_arg =
   | EO_expr of expr
   | EO_op_arg of op_arg



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
 and assume_prove = {
     location          : location;
     level             : level option;
     new_symbols       : new_symb list;
     assumes           : assume_prove list;
     prove             : expr;
   }

 (** The NEW keyword of TLA, optionally with the containing set.
    Example: NEW STATE s \in States
  *)
 and new_symb = {
     location          : location;
     level             : level option;
     op_decl           : op_decl;
     set               : expr option;
   }


 (** An operator definition, either by instantiating a module,
    a user defined operator or a builtin operator.
  *)
 and op_def =
   | O_user_defined_op of user_defined_op
   | O_builtin_op of builtin_op


 (** A user defined operator or a reference to it.
  *)
 and user_defined_op =
   | UOP_ref of int (*make it int * string, but makes conversion more complex *)
   | UOP of user_defined_op_

 (** A user defined operator in TLA.
    Example:
    Op(x,y) == ENABLED (x' # y')
  *)
 and user_defined_op_ = {
     location          : location;
     level             : level option;
     name              : string;
     arity             : int;
     body              : expr;
     params            : formal_param list;
     recursive         : bool;
   }

 (** A lambda abstraction. The difference to binders is that it works on formal
    parameters, not variables.
  *)
 and lambda = {
     (*TODO: check if a lambda abstraction has a level*)
     location          : location;
     level             : level option;
     arity             : int;
     body              : expr;
     params            : (formal_param * bool (*is leibniz*)) list;
   }

 (** A builtin operator of TLA. See the TLA book p. 268ff. for a list.
    Each operator is a constant.
    Example: =>, TRUE
  *)
 and builtin_op = {
     level             : level option;
     name              : string;
     arity             : int;
     params            : (formal_param * bool (*is leibniz*)) list
   }

 (** An argument for an operator. Apparently the arity is always >0.
    TODO: check what the difference to a formal parameter is.
  *)
 and op_arg = {
     location          : location;
     level             : level option;
     (*  name              : string; *)
     (* arity             : int; *)
     argument          : operator;
   }

 (** A formal parameter or a reference to it.*)
 and formal_param =
   | FP_ref of int
   | FP of formal_param_

 (** Represents arguments of a declared operator.
    Example: x and y in
     Op(x,y) = x + y
  *)
 and formal_param_ = {
     location          : location;
     level             : level option; (* \A x : x = x' is provable because of the level of x. make sure the level is checked *)
     name              : string;
     arity             : int
   }

 (** An operator declaration or a reference to it. *)
 and op_decl =
   | OPD_ref of int
   | OPD of op_decl_

 (** An operator declaration, including operators introduced by NEW.
    Example:
     VARIABLES x,y
     ASSUME NEW P(_), NEW VARIABLE x PROVE P(x)
  *)
 and op_decl_ = {
     location          : location;
     level             : level option;
     name              : string;
     arity             : int;
     kind              : op_decl_kind
   }


 (** The union of expressions, modules and module instances. *)
 and expr_or_module_or_module_instance = (* TODO: remove *)
   | EMM_expr of expr

 (** An defintion which can be expanded. Either a user defined operator,
    a module instance, a theorem or an assumption.
  *)
 and defined_expr =
   | UMTA_user_defined_op of user_defined_op


 and op_appl_or_binder =
   | OB_op_appl of op_appl
   | OB_binder of binder

 and at = {
     location          : location;
     level             : level option;
     except            : op_appl_or_binder;
     except_component  : op_appl_or_binder
   }

 and decimal = {
     location          : location;
     level             : level option;
     mantissa          : int;
     exponent          : int
   }

 and label = {
     location          : location;
     level             : level option;
     name              : string;
     arity             : int;
     body              : assume_prove;
     params            : formal_param list
   }

 and op_def_or_theorem_or_assume = (* TODO remove *)
   | OTA_op_def of op_def

 (** The LET statement of TLA.
    Example:
    LET T == A /\ B IN T => T
  *)
 and let_in = {
     location          : location;
     level             : level option;
     body              : expr;
     op_defs           : op_def_or_theorem_or_assume list
   }

 and numeral = {
     location          : location;
     level             : level option;
     value             : int
   }

 and strng = {
     location          : location;
     level             : level option;
     value             : string
   }

 (**
   Operator corresponds to {!module:Sany_ds}'s disjunction type
   formal_param_or_module_or_op_decl_or_op_def_or_theorem_or_assume_or_apsubst.
   An operator is anything which can have arguments applied.
  *)
 and operator =
   | FMOTA_formal_param of formal_param
   | FMOTA_op_decl of op_decl
   | FMOTA_op_def of op_def

 and op_appl = {
     location          : location;
     level             : level option;
     operator          : operator;
     operands          : expr_or_op_arg list;
   }

 (**
 A binder represents a term (B x: F[x]) where B is one of
 \A,\E,\AA,\EE and x is a bound variable. It can be used wherever
 an application is possible.
  *)
 and binder = {
     location          : location;
     level             : level option;
     operator          : operator;
     (* TODO:  check if we really only need one operand *)
     operand           : expr_or_op_arg;
     bound_symbols     : bound_symbol list
   }


 and bound_symbol =
   | B_unbounded_bound_symbol of unbounded_bound_symbol
   | B_bounded_bound_symbol of bounded_bound_symbol

 and unbounded_bound_symbol = {
     param             : formal_param;
     tuple             : bool
   }

 and bounded_bound_symbol = {
     params            : formal_param list;
     tuple             : bool;
     domain            : expr
   }

and mule_entry =
  | MODe_op_decl of op_decl
  | MODe_op_def of op_def


type entry =
  FP_entry of formal_param_ |
  OPDec_entry of op_decl_ |
  OPDef_entry of op_def


type term_db = (int * entry) list

