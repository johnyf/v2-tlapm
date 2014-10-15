
(* Copyright (C) 2014 MSR-INRIA
 *
 * SANY expressions
 *
 * Author: TL
 *)

open Commons

type node =
  | APSubstIn of ap_subst_in
  | AssumeProve of assume_prove
  | DefStep of def_step
  | Expr of expr
  | OpArg of op_arg
  | Instance of instance
  | NewSymb of new_symb
  | Proof of proof
  | FormalParam of formal_param
  | Module of mule
  | OpDecl of op_decl
  | OpDef of op_def
  | Assume of assume
  | Theorem of theorem
  | UseOrHide of use_or_hide

and expr =
  | At of at
  | Decimal of decimal
  | Label of label
  | LetIn of let_in
  | Numeral of numeral
  | OpAppl of op_appl
  | String of strng
  | SubsrtIn of subst_in

and ap_subst_in = {
  loc               : location;
  lvl               : level;
  substs            : subst list;
  body              : node
}

and subst_in = {
  loc               : location;
  lvl               : level;
  substs            : subst list;
  body              : expr
}

and instance = {
  loc               : location;
  lvl               : level;
  name              : string;
  substs            : subst list;
  params            : formal_param list
}

and subst = {
  loc               : location;
  lvl               : level;

}

and assume = {
  loc               : location;
  lvl               : level;

}

and theorem = {
  loc               : location;
  lvl               : level;

}

and assume_prove = {
  loc               : location;
  lvl               : level;

}

and new_symb = {
  loc               : location;
  lvl               : level;

}

and op_def =
  | ModuleInstance of module_instance
  | UserDefinedOp of user_defined_op
  | BuiltinOp of builtin_op

and module_instance = {
  loc               : location;
  lvl               : level;

}

and user_defined_op = {
  loc               : location;
  lvl               : level;

}

and builtin_op = {
  loc               : location;
  lvl               : level;

}

and op_arg = {
  loc               : location;
  lvl               : level;

}

and formal_param = {
  loc               : location;
  lvl               : level;

}

and op_decl = {
  loc               : location;
  lvl               : level;

}

and proof = {
  loc               : location;
  lvl               : level;

}

and omitted = {
  loc               : location;
  lvl               : level;

}

and obvious = {
  loc               : location;
  lvl               : level;

}

and by = {
  loc               : location;
  lvl               : level;

}

and step =
  | DefStep of def_step
  | UseOrHide of use_or_hide
  | Instance of instance
  | Theorem of theorem

and def_step = {
  loc               : location;
  lvl               : level;

}

and use_or_hide = {
  loc               : location;
  lvl               : level;

}

and at = {
  loc               : location;
  lvl               : level;

}

and decimal = {
  loc               : location;
  lvl               : level;

}

and label = {
  loc               : location;
  lvl               : level;

}

and let_in = {
  loc               : location;
  lvl               : level;

}

and numeral = {
  loc               : location;
  lvl               : level;

}

and strng = {
  loc               : location;
  lvl               : level;

}

and op_appl = {
  loc               : location;
  lvl               : level;

}

(* modules *)
and mule = {
  name              : string;
  loc               : location;
  constants         : op_decl list;
  variables         : op_decl list;
  definitions       : op_def list ;
  assumptions       : assume list ;
  theorems          : theorem list ;
}

