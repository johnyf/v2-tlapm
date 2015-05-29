open Commons

(**
   These datastructures directly mirror the XML schema in lib/sany.xsd. There
   are two differences:

  {ol
   {- The handling of references is realized such that each of the types
   allowing a reference are disjunctions of either a reference or a concrete
   record. }
   {- The handling of references is realized such that each of the types
   allowing a reference are disjunctions of either a reference or a concrete
   record. }
  }

   Additional disjunction types (like expr_or_op_arg) are necessary, whenever
   the XML schema may contain child nodes of different types. The have the form
   type1_or_..._or_typeN.
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

and expr_or_assume_prove =
  | EA_expr of expr
  | EA_assume_prove of assume_prove

and  new_symb_or_expr_or_assume_prove =
  | NEA_new_symb of new_symb
  | NEA_expr of expr
  | NEA_assume_prove of assume_prove

and ap_subst_in = {
  location          : location option;
  level             : level option;
  substs            : subst list;
  body              : node
}

and subst_in = {
  location          : location option;
  level             : level option;
  substs            : subst list;
  body              : expr
}

and instance = {
  location          : location option;
  level             : level option;
  name              : string;
  substs            : subst list;
  params            : formal_param list
}

and subst = {
  (*  location          : location option; *)
  (*  level             : level option;    *)
  op                : op_decl;
  expr              : expr_or_op_arg
}

and assume =
| ASSUME_ref of int
| ASSUME of assume_

and assume_ = {
  location          : location option;
  level             : level option;
  expr              : expr
}

and theorem =
| THM_ref of int
| THM of theorem_

and theorem_ = {
  location          : location option;
  level             : level option;
  name              : string option;
  expr              : expr_or_assume_prove;
  proof             : proof;
  suffices          : bool
}

and assume_prove = {
  location          : location option;
  level             : level option;
  assumes           : new_symb_or_expr_or_assume_prove list;
  prove             : expr;
  suffices          : bool;
  boxed             : bool
}

and new_symb = {
  location          : location option;
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
  location          : location option;
  level             : level option;
  name              : string
}

and user_defined_op =
  | UOP_ref of int
  | UOP of user_defined_op_

and user_defined_op_ = {
  location          : location option;
  level             : level option;
  name              : string;
  arity             : int;
  body              : expr;
  params            : (formal_param * bool (*is leibniz*)) list;
  recursive         : bool
}

and builtin_op =
  | BOP_ref of int
  | BOP of builtin_op_

and builtin_op_ = {
  location          : location option;
  level             : level option;
  name              : string;
  arity             : int;
  params            : (formal_param * bool (*is leibniz*)) list
}

and op_arg = {
  location          : location option;
  level             : level option;
  name              : string;
  arity             : int
}

and formal_param =
   | FP_ref of int
   | FP of formal_param_

and formal_param_ = {
     location          : location option;
     level             : level option;
     name              : string;
     arity             : int
}

and op_decl =
   | OPD_ref of int
   | OPD of op_decl_

and op_decl_ = {
  location          : location option;
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
  location          : location option;
  level             : level option;
}

and obvious = {
  location          : location option;
  level             : level option;
}

and expr_or_module_or_module_instance =
  | EMM_expr of expr
  | EMM_module of mule
  | EMM_module_instance of module_instance

and user_defined_op_or_module_instance_or_theorem_or_assume =
  | UMTA_user_defined_op of user_defined_op
  | UMTA_module_instance of module_instance
  | UMTA_theorem of theorem
  | UMTA_assume of assume

and by = {
  location       : location option;
  level          : level option;
  facts          : expr_or_module_or_module_instance list;
  defs           : user_defined_op_or_module_instance_or_theorem_or_assume list;
  only           : bool
}

and steps = {
  location       : location option;
  level          : level option;
  steps          : step list
}

and step =
  | S_def_step of def_step
  | S_use_or_hide of use_or_hide
  | S_instance of instance
  | S_theorem of theorem

and def_step = {
  location       : location option;
  level          : level option;
  defs           : op_def list
}

and use_or_hide = {
  location       : location option;
  level          : level option;
  facts          : expr_or_module_or_module_instance list;
  defs           : user_defined_op_or_module_instance_or_theorem_or_assume list;
  only           : bool;
  hide           : bool
}

and at = {
  location          : location option;
  level             : level option;
  except            : op_appl;
  except_component  : op_appl
}

and decimal = {
  location          : location option;
  level             : level option;
  mantissa          : int;
  exponent          : int
}

and label = {
  location          : location option;
  level             : level option;
  name              : string;
  arity             : int;
  body              : expr_or_assume_prove;
  params            : formal_param list
}

and op_def_or_theorem_or_assume =
  | OTA_op_def of op_def
  | OTA_theorem of theorem
  | OTA_assume of assume

and let_in = {
  location          : location option;
  level             : level option;
  body              : expr;
  op_defs           : op_def_or_theorem_or_assume list
}

and numeral = {
  location          : location option;
  level             : level option;
  value             : int
}

and strng = {
  location          : location option;
  level             : level option;
  value             : string
}

and formal_param_or_module_or_op_decl_or_op_def_or_theorem_or_assume_or_apsubst =
  | FMOTA_formal_param of formal_param
  | FMOTA_module of mule
  | FMOTA_op_decl of op_decl
  | FMOTA_op_def of op_def
  | FMOTA_theorem of theorem
  | FMOTA_assume of assume
  | FMOTA_ap_subst_in of ap_subst_in

and op_appl = {
  location          : location option;
  level             : level option;
  operator          : formal_param_or_module_or_op_decl_or_op_def_or_theorem_or_assume_or_apsubst;
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
  location          : location option;
  constants         : op_decl list;
  variables         : op_decl list;
  definitions       : op_def list ;
  assumptions       : assume list ;
  theorems          : theorem list ;
}

  (* context *)
type entry = {
  uid       : int;
  reference : formal_param_or_module_or_op_decl_or_op_def_or_theorem_or_assume_or_apsubst;
}

type context = {
  entries : entry list;
  modules : mule list;
}
