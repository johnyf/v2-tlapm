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

(** Represents a node which can be instantiated via ap_subst_in*)
type node =
  | N_ap_subst_in of ap_subst_in
  | N_assume_prove of assume_prove
  | N_def_step of def_step
  | N_expr of expr
  | N_op_arg of op_arg
  | N_instance of instance
  | N_new_symb of new_symb
  | N_proof of proof
  | N_formal_param of formal_param
  | N_module of mule
  | N_op_decl of op_decl
  | N_op_def of op_def
  | N_assume of assume
  | N_theorem of theorem
  | N_use_or_hide of use_or_hide

 (** Represents a TLA expression.
    Example:
    f(1 + "sde")
  *)
 and expr =
   | E_at of at
   | E_decimal of decimal
   | E_label of label
   | E_let_in of let_in
   | E_numeral of numeral
   | E_op_appl of op_appl
   | E_string of strng
   | E_subst_in of subst_in
   | E_binder of binder
   | E_lambda of lambda

 (** The union of expressions and operator arguments.
    Used by substitutions and as operand in applications. *)
 and expr_or_op_arg =
   | EO_expr of expr
   | EO_op_arg of op_arg

 (** An instantiation of an ASSUME - PROVE expression. Other
    nodes are also allowed in SANY although it is only inteded to be used
    with ASSUME - PROVE.
    (TODO: check the SANY implementation what is really generated)
  *)
 and ap_subst_in = {
     location          : location;
     level             : level option;
     substs            : instantiation list;
     body              : node
   }

 (** An instantiation of an expression. Could be expressed
    as an ap_subst_in, but is easier to treat because we
    know more about the body.
  *)
 and subst_in = {
     location          : location;
     level             : level option;
     substs            : instantiation list;
     body              : expr
   }

 (** An instance step within a proof. The effect is the same
    as using a global definition.
  *)
 and instance = {
     location          : location;
     level             : level option;
     name              : string option;
     module_name       : string;
     substs            : instantiation list;
     params            : formal_param list
   }

 (** An instantiation replaces rigid or flexible variables (both of type op_decl)
     by an expression. In contrast to a substitution of a formal parameter,
     instantiation must preserve the validity of the formula. This is visible
     when it is applied to an operator argument without the Leibniz property
     like ENABLED.

     E.g.:
     Suppose the module Foo defines:

      VARIABLE x

      Itchy == ENABLED (x # x')

      Scratchy(u) == ENABLED (u # u')

     Then we instantiate Foo in the module Bar:

      VARIABLE u

      INSTANCE I == Foo with x <- u

     The expression I!Itchy is supposed be equivalent to

       ENABLED( u # up')

     where up is a fresh variable.

     At the same time, I!Scratchy(u) is supposed be equivalent to:

       ENABLED( u # u')

     Remark: instantiations are used with instance, subst_in and ap_subst_in.
             substitutions are used when reducing lambda expressions,
             unfolding definitions and creating instances of quantifiers
             (e.g. in universal elimination where from \A x:P(x) we deduce
                   P(t) )
  *)
 and instantiation = {
     op                : op_decl;
     expr              : expr_or_op_arg
   }

 (** The union of assumption statements and references to them. *)
 and assume =
   | ASSUME_ref of int
   | ASSUME of assume_

 (** The ASSUME statement of TLA.
    Example: ASSUME x > 0
  *)
 and assume_ = {
     location          : location;
     level             : level option;
     expr              : expr
   }

 (** The union of theorem statements and references to them. *)
 and theorem =
   | THM_ref of int
   | THM of theorem_

 (** The THEOREM statement of TLA.
    Example:
    THEOREM ASSUME NEW x, x > 0 PROVE -x <0 BY SMT
  *)
 and theorem_ = {
     location          : location;
     level             : level option;
     name              : string option;
     statement         : statement;
     proof             : proof;
   }

 and statement =
   (* these statements have proofs *)
   | ST_FORMULA of assume_prove
   | ST_SUFFICES of assume_prove
   | ST_CASE of expr
   | ST_PICK of pick
   | ST_QED
   (* these don't have proofs but are represented as theorems in SANY
    *)
   | ST_HAVE of expr
   | ST_TAKE of expr
   | ST_WITNESS of expr

 and pick = {
     variables : bound_symbol list;
     formula  : expr;
   }

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
     suffices          : bool;
     boxed             : bool
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
   | O_module_instance of module_instance
   | O_user_defined_op of user_defined_op
   | O_builtin_op of builtin_op

 (** A module instantiation or a reference to one. *)
 and module_instance =
   | MI_ref of int
   | MI of module_instance_

 (** An operator representing an instantiated module.
    Example:
     I(x) == INSTANTIATE Mod WITH k <- x, n <- 2*x.

     The module argument is changed to an additional operator
     argument of a definition in Mod:
     I(x)!P(y) is represented as I!P(x,y)
  *)
 and module_instance_ = {
     location          : location;
     level             : level option;
     name              : string
   }

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
     params            : (formal_param * bool (*is leibniz*)) list;
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

 (** The different kinds of proofs. Corresponds to the
    keywords OMITTED, OBVIOUS and BY as well to steps
    (e.g. <1>2). No_proof is similar to OMITTED, but
    will be colored differently in the toolbox.
  *)
 and proof =
   | P_omitted of omitted
   | P_obvious of obvious
   | P_by of by
   | P_steps of steps
   | P_noproof (* if the theorem has no proof - should be treated like omitted *)

 (** Represents the OMITTED proof statement in TLA. *)
 and omitted = {
     location          : location;
     level             : level option;
   }

 (** Represents the OBVIOUS proof statement in TLA. *)
 and obvious = {
     location          : location;
     level             : level option;
   }

 (** The union of expressions, modules and module instances. *)
 and expr_or_module_or_module_instance =
   | EMM_expr of expr
   | EMM_module of mule
   | EMM_module_instance of module_instance

 (** An defintion which can be expanded. Either a user defined operator,
    a module instance, a theorem or an assumption.
  *)
 and defined_expr =
   | UMTA_user_defined_op of user_defined_op
   | UMTA_module_instance of module_instance
   | UMTA_theorem of theorem
   | UMTA_assume of assume

 (** The BY statment for a proof. The only flag differentiates between
    BY and BY ONLY proofs. See section 7.2.2 of the tla2 guide for
    an explanation.
    Example:
    P(x) == x+2
    THEOREM PROVE P(3) = 5 BY SMT DEF P
  *)
 and by = {
     location       : location;
     level          : level option;
     facts          : expr_or_module_or_module_instance list;
     defs           : defined_expr list;
     only           : bool
   }

 (** A proof by steps.
    Example:
     THEOREM ASSUME NEW P, NEW Q PROVE P /\ Q => P \/ Q
     <1>1. P => P \/ Q OBVIOUS
     <1>2. Q => P \/ Q OBVIOUS
     <1> QED BY <1>1, <1>2
  *)
 and steps = {
     location       : location;
     level          : level option;
     steps          : step list
   }

 and step =
   | S_def_step of def_step
   | S_use_or_hide of use_or_hide
   | S_instance of instance
   | S_theorem of theorem

 and def_step = {
     location       : location;
     level          : level option;
     defs           : op_def list
   }

 and use_or_hide = {
     location       : location;
     level          : level option;
     facts          : expr_or_module_or_module_instance list;
     defs           : defined_expr list;
     only           : bool;
     hide           : bool
   }

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

 and op_def_or_theorem_or_assume =
   | OTA_op_def of op_def
   | OTA_theorem of theorem
   | OTA_assume of assume

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
   | FMOTA_formal_param of formal_param (* has any arity *)
   | FMOTA_module of mule               (* has arity 0 *)
   | FMOTA_op_decl of op_decl           (* has any arity *)
   | FMOTA_op_def of op_def             (* has any arity *)
   | FMOTA_theorem of theorem           (* has arity 0 *)
   | FMOTA_assume of assume             (* has arity 0 *)
   | FMOTA_ap_subst_in of ap_subst_in   (* has arity 0 *)

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
  | MODe_assume of assume
  | MODe_theorem of theorem
  | MODe_use_or_hide of use_or_hide
  | MODe_instance of instance

 (* modules *)
 and mule =
   | MOD_ref of int
   | MOD of mule_

 and mule_ = {
     name              : string;
     location          : location;
     module_entries    : mule_entry list;
   }

type entry =
  FP_entry of formal_param_ |
  MOD_entry of mule_ |
  OPDec_entry of op_decl_ |
  OPDef_entry of op_def |
  THM_entry of theorem_ |
  ASSUME_entry of assume_ |
  APSUBST_entry of ap_subst_in


type term_db = (int * entry) list

(** In contrast to the {!module:Sany_ds} context, entries
    for different elements are separate.
 *)
type context = {
    root_module : string;
    entries : term_db;
    modules : mule list;
  }
