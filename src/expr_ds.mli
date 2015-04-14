open Commons

(**
   These datastructures are close to the xml parser datastructures in sany_ds.
   The differences are:
   1) instead of location option we use dummy values (Commons.mkDummyLocation)
   2) the expr in expr_or_assume_prove is replaced by assume_prove without
      assumptions
   3) new_symb_or_expr_or_assume_prove is reduced to assume_prove, similar to 2
      the new symbols are stored in a separate list in assume prove. the order
      in which they are declared is only relevant for the sany parser.
   4) builtin operator references are expanded to applications of $builtinname.
      the expr_visitor will provide callbacks for each builtin.

      builtins don't have a location anymore.
   5) entries in the context are now seperated by type
   6) module instance references are unfolded.
   7) operator definition references contain the name
   8) Renamings:
     formal_param_or_module_or_op_decl_or_op_def_or_theorem_or_assume_or_apsubst = operator
     user_defined_op_or_module_instance_or_theorem_or_assume = defined_expr

   Still open:
   -) what to do about references? by default, we don't unfold definitions, etc.
      does it make so much more sense to refer to them by name instead of ints?
      this only works for named objects like definitions, but not for unnamed
      theorems etc.
   -) Module instance refer to modules by name, but they don't exist in the
      representation anymore.
      At the moment, we try to infer the module name from the definition name.

*)

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

and expr =
  | E_at of at
  | E_decimal of decimal
  | E_label of label
  | E_let_in of let_in
  | E_numeral of numeral
  | E_op_appl of op_appl
  | E_string of strng
  | E_subst_in of subst_in

and expr_or_op_arg =
  | EO_expr of expr
  | EO_op_arg of op_arg

and ap_subst_in = {
  location          : location;
  level             : level option;
  substs            : subst list;
  body              : node
}

and subst_in = {
  location          : location;
  level             : level option;
  substs            : subst list;
  body              : expr
}

and instance = {
  location          : location;
  level             : level option;
  name              : string;
  substs            : subst list;
  params            : formal_param list
}

and subst = {
  op                : op_decl;
  expr              : expr_or_op_arg
}

and assume =
| ASSUME_ref of int
| ASSUME of assume_

and assume_ = {
  location          : location;
  level             : level option;
  expr              : expr
}

and theorem =
| THM_ref of int
| THM of theorem_

and theorem_ = {
  location          : location;
  level             : level option;
  expr              : assume_prove;
  proof             : proof;
  suffices          : bool
}

and assume_prove = {
  location          : location;
  level             : level option;
  new_symbols       : new_symb list;
  assumes           : assume_prove list;
  prove             : expr;
  suffices          : bool;
  boxed             : bool
}

and new_symb = {
  location          : location;
  level             : level option;
  op_decl           : op_decl;
  set               : expr option;
}

and op_def =
  | OPDef_ref of int
  | OPDef of op_def_

and op_def_ =
  | O_module_instance of module_instance
  | O_user_defined_op of user_defined_op
  | O_builtin_op of builtin_op

and module_instance =
  | MI_ref of int
  | MI of module_instance_

and module_instance_ = {
  location          : location;
  level             : level option;
  name              : string
}

and user_defined_op =
  | UOP_ref of (int * string)
  | UOP of user_defined_op_

and user_defined_op_ = {
  location          : location;
  level             : level option;
  name              : string;
  arity             : int;
  body              : expr;
  params            : (formal_param * bool (*is leibniz*)) list;
  recursive         : bool
}

and builtin_op = {
  level             : level option;
  name              : string;
  arity             : int;
  params            : (formal_param * bool (*is leibniz*)) list
}

and op_arg = {
  location          : location;
  level             : level option;
  name              : string;
  arity             : int
}

and formal_param =
   | FP_ref of int
   | FP of formal_param_

and formal_param_ = {
     location          : location;
     level             : level option;
     name              : string;
     arity             : int
}

and op_decl =
   | OPD_ref of int
   | OPD of op_decl_

and op_decl_ = {
  location          : location;
  level             : level option;
  name              : string;
  arity             : int;
  kind              : op_decl_kind
}

and proof =
  | P_omitted of omitted
  | P_obvious of obvious
  | P_by of by
  | P_steps of steps
  | P_noproof (* if the theorem has no proof -- should be treated like omitted *)

and omitted = {
  location          : location;
  level             : level option;
}

and obvious = {
  location          : location;
  level             : level option;
}

and expr_or_module_or_module_instance =
  | EMM_expr of expr
  | EMM_module of mule
  | EMM_module_instance of module_instance

and defined_expr =
  | UMTA_user_defined_op of user_defined_op
  | UMTA_module_instance of module_instance
  | UMTA_theorem of theorem
  | UMTA_assume of assume

and by = {
  location       : location;
  level          : level option;
  facts          : expr_or_module_or_module_instance list;
  defs           : defined_expr list;
  only           : bool
}

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

and at = {
  location          : location;
  level             : level option;
  except            : op_appl;
  except_component  : op_appl
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
   Operator corresponds is Sany_ds's disjunction type
   formal_param_or_module_or_op_decl_or_op_def_or_theorem_or_assume_or_apsubst.
   An operator is anything which can have arguments applied.
*)
and operator =
  | FMOTA_formal_param of formal_param
  | FMOTA_module of mule
  | FMOTA_op_decl of op_decl
  | FMOTA_op_def of op_def
  | FMOTA_theorem of theorem
  | FMOTA_assume of assume
  | FMOTA_ap_subst_in of ap_subst_in

and op_appl = {
  location          : location;
  level             : level option;
  operator          : operator;
  operands          : expr_or_op_arg list;
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

(* modules *)
and mule =
  | MOD_ref of int
  | MOD of mule_

and mule_ = {
  name              : string;
  location          : location;
  constants         : op_decl list;
  variables         : op_decl list;
  definitions       : op_def list ;
  assumptions       : assume list ;
  theorems          : theorem list ;
}

type context = {
  fp_entries      : (int * formal_param_) list;
  mod_entries     : (int * mule_) list;
  opdec_entries   : (int * op_decl_) list;
  opdef_entries   : (int * op_def_) list;
  theorem_entries : (int * theorem_) list;
  assume_entries  : (int * assume_) list;
  apsubst_entries : (int * ap_subst_in) list;
  modules : mule list;
}
(*  *)
