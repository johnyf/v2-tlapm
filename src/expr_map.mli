open Commons
open Expr_ds
open Expr_visitor


type _ anyExpr =
  | Nothing : unit -> unit anyExpr
  | Any_location : location -> location anyExpr
  | Any_level  : level -> level anyExpr
  | Any_name  : string -> string anyExpr
  | Any_reference  : int -> int anyExpr
  | Any_node  : node -> node anyExpr
  | Any_expr  : expr -> expr anyExpr
  | Any_expr_or_oparg  : expr_or_op_arg -> expr_or_op_arg anyExpr
  | Any_ap_subst_in  : ap_subst_in -> ap_subst_in anyExpr
  | Any_subst_in  : subst_in -> subst_in anyExpr
  | Any_instance  : instance -> instance anyExpr
  | Any_subst  : subst -> subst anyExpr
  | Any_assume  : assume -> assume anyExpr
  | Any_assume_  : assume_ -> assume_ anyExpr
  | Any_theorem  : theorem -> theorem anyExpr
  | Any_theorem_  : theorem_ -> theorem_ anyExpr
  | Any_assume_prove  : assume_prove -> assume_prove anyExpr
  | Any_new_symb  : new_symb -> new_symb anyExpr
  | Any_op_def  : op_def -> op_def anyExpr
  | Any_op_def_  : op_def_ -> op_def_ anyExpr
  | Any_module_instance  : module_instance -> module_instance anyExpr
  | Any_module_instance_  : module_instance_ -> module_instance_ anyExpr
  | Any_user_defined_op  : user_defined_op -> user_defined_op anyExpr
  | Any_user_defined_op_  : user_defined_op_ -> user_defined_op_ anyExpr
  | Any_builtin_op  : builtin_op -> builtin_op anyExpr
  | Any_op_arg  : op_arg -> op_arg anyExpr
  | Any_formal_param  : formal_param -> formal_param anyExpr
  | Any_formal_param_  : formal_param_ -> formal_param_ anyExpr
  | Any_op_decl  : op_decl -> op_decl anyExpr
  | Any_op_decl_  : op_decl_ -> op_decl_ anyExpr
  | Any_proof : proof -> proof anyExpr
  | Any_omitted  : omitted -> omitted anyExpr
  | Any_obvious  : obvious -> obvious anyExpr
  | Any_expr_or_module_or_module_instance  :
      expr_or_module_or_module_instance -> expr_or_module_or_module_instance anyExpr
  | Any_defined_expr  : defined_expr -> defined_expr anyExpr
  | Any_by  : by -> by anyExpr
  | Any_steps  : steps -> steps anyExpr
  | Any_step  : step -> step anyExpr
  | Any_def_step  : def_step -> def_step anyExpr
  | Any_use_or_hide  : use_or_hide -> use_or_hide anyExpr
  | Any_at  : at -> at anyExpr
  | Any_decimal  : decimal -> decimal anyExpr
  | Any_label  : label -> label anyExpr
  | Any_op_def_or_theorem_or_assume  :
      op_def_or_theorem_or_assume -> op_def_or_theorem_or_assume anyExpr
  | Any_let_in  : let_in -> let_in anyExpr
  | Any_numeral  : numeral -> numeral anyExpr
  | Any_strng  : strng -> strng anyExpr
  | Any_operator  : operator -> operator anyExpr
  | Any_op_appl  : op_appl -> op_appl anyExpr
  | Any_binder  : binder -> binder anyExpr
  | Any_lambda  : lambda -> lambda anyExpr
  | Any_bound_symbol  : bound_symbol -> bound_symbol anyExpr
  | Any_unbounded_bound_symbol  :
      unbounded_bound_symbol -> unbounded_bound_symbol anyExpr
  | Any_bounded_bound_symbol  :
      bounded_bound_symbol -> bounded_bound_symbol anyExpr
  | Any_mule  : mule -> mule anyExpr
  | Any_mule_  : mule_ -> mule_ anyExpr
  | Any_context  : context -> context anyExpr
  | Any_entry  : entry -> entry anyExpr

type ('a, 'b) macc =  'a anyExpr * 'b

class ['a,'b] expr_map : object
  method expr            : ('a, 'b) macc -> expr -> ('a, 'b) macc
  method name            : ('a, 'b) macc -> string -> ('a, 'b) macc
  method location        : ('a, 'b) macc -> location -> ('a, 'b) macc
  method level           : ('a, 'b) macc -> level option -> ('a, 'b) macc
  method decimal         : ('a, 'b) macc -> decimal -> ('a, 'b) macc
  method numeral         : ('a, 'b) macc -> numeral -> ('a, 'b) macc
  method strng           : ('a, 'b) macc -> strng -> ('a, 'b) macc
  method at              : ('a, 'b) macc -> at -> ('a, 'b) macc
  method op_appl         : ('a, 'b) macc -> op_appl -> ('a, 'b) macc
  method binder          : ('a, 'b) macc -> binder -> ('a, 'b) macc
  method op_arg          : ('a, 'b) macc -> op_arg -> ('a, 'b) macc
  method operator        : ('a, 'b) macc -> operator -> ('a, 'b) macc
  method expr_or_op_arg  : ('a, 'b) macc -> expr_or_op_arg -> ('a, 'b) macc
  method bound_symbol    : ('a, 'b) macc -> bound_symbol -> ('a, 'b) macc
  method bounded_bound_symbol   : ('a, 'b) macc -> bounded_bound_symbol -> ('a, 'b) macc
  method unbounded_bound_symbol : ('a, 'b) macc -> unbounded_bound_symbol -> ('a, 'b) macc
  method mule            : ('a, 'b) macc -> mule -> ('a, 'b) macc
  method formal_param    : ('a, 'b) macc -> formal_param -> ('a, 'b) macc
  method op_decl         : ('a, 'b) macc -> op_decl -> ('a, 'b) macc
  method op_def          : ('a, 'b) macc -> op_def -> ('a, 'b) macc
  method theorem         : ('a, 'b) macc -> theorem -> ('a, 'b) macc
  method assume          : ('a, 'b) macc -> assume -> ('a, 'b) macc
  method assume_prove    : ('a, 'b) macc -> assume_prove -> ('a, 'b) macc
  method new_symb        : ('a, 'b) macc -> new_symb -> ('a, 'b) macc
  method ap_subst_in     : ('a, 'b) macc -> ap_subst_in -> ('a, 'b) macc
  method module_instance : ('a, 'b) macc -> module_instance -> ('a, 'b) macc
  method builtin_op      : ('a, 'b) macc -> builtin_op -> ('a, 'b) macc
  method user_defined_op : ('a, 'b) macc -> user_defined_op -> ('a, 'b) macc
  method proof           : ('a, 'b) macc -> proof -> ('a, 'b) macc
  method step            : ('a, 'b) macc -> step -> ('a, 'b) macc
  method instance        : ('a, 'b) macc -> instance -> ('a, 'b) macc
  method use_or_hide     : ('a, 'b) macc -> use_or_hide -> ('a, 'b) macc
  method subst           : ('a, 'b) macc -> subst -> ('a, 'b) macc
  method label           : ('a, 'b) macc -> label -> ('a, 'b) macc
  method let_in          : ('a, 'b) macc -> let_in -> ('a, 'b) macc
  method subst_in        : ('a, 'b) macc -> subst_in -> ('a, 'b) macc
  method node            : ('a, 'b) macc -> node -> ('a, 'b) macc
  method def_step        : ('a, 'b) macc -> def_step -> ('a, 'b) macc
  method reference       : ('a, 'b) macc -> int -> ('a, 'b) macc

  method entry           : ('a, 'b) macc -> (int * entry) -> ('a, 'b) macc
  method context         : ('a, 'b) macc -> context -> ('a, 'b) macc

  method op_appl_or_binder : ('a, 'b) macc -> op_appl_or_binder -> ('a, 'b) macc
  method expr_or_module_or_module_instance :
           ('a, 'b) macc -> expr_or_module_or_module_instance -> ('a, 'b) macc
  method defined_expr : ('a, 'b) macc -> defined_expr -> ('a, 'b) macc
  method op_def_or_theorem_or_assume       :
           ('a, 'b) macc -> op_def_or_theorem_or_assume -> ('a, 'b) macc

end
