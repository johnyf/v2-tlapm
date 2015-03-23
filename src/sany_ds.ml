open Commons

(* type declarations *)
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
  | P_noproof  

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
  location          : location option;
  level             : level option;
  facts             : expr_or_module_or_module_instance list;
  defs              : user_defined_op_or_module_instance_or_theorem_or_assume list;
  only              : bool
}

and steps = {
  location          : location option;
  level             : level option;
  steps             : step list
}

and step =
  | S_def_step of def_step
  | S_use_or_hide of use_or_hide
  | S_instance of instance
  | S_theorem of theorem

and def_step = {
  location          : location option;
  level             : level option;
  defs              : op_def list
}

and use_or_hide = {
  location          : location option;
  level             : level option;
  facts             : expr_or_module_or_module_instance list;
  defs              : user_defined_op_or_module_instance_or_theorem_or_assume list;
  only              : bool;
  hide              : bool
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
(* end of type declarations *)
  
class ['a] visitor :
object
  method expr            : 'a -> expr -> 'a
  method name            : 'a -> string -> 'a
  method location        : 'a -> location option -> 'a
  method level           : 'a -> level option -> 'a
  method decimal         : 'a -> decimal -> 'a
  method numeral         : 'a -> numeral -> 'a
  method strng           : 'a -> strng -> 'a
  method at              : 'a -> at -> 'a
  method op_appl         : 'a -> op_appl -> 'a
  method op_arg          : 'a -> op_arg -> 'a
  method fmota           : 'a -> formal_param_or_module_or_op_decl_or_op_def_or_theorem_or_assume_or_apsubst -> 'a
  method expr_or_op_arg  : 'a -> expr_or_op_arg -> 'a
  method bound_symbol    : 'a -> bound_symbol -> 'a
  method bounded_bound_symbol   : 'a -> bounded_bound_symbol -> 'a
  method unbounded_bound_symbol : 'a -> unbounded_bound_symbol -> 'a
  method mule            : 'a -> mule -> 'a
  method formal_param    : 'a -> formal_param -> 'a
  method op_decl         : 'a -> op_decl -> 'a
  method op_def          : 'a -> op_def -> 'a
  method theorem         : 'a -> theorem -> 'a
  method assume          : 'a -> assume -> 'a
  method assume_prove    : 'a -> assume_prove -> 'a
  method new_symb        : 'a -> new_symb -> 'a
  method ap_subst_in     : 'a -> ap_subst_in -> 'a
  method module_instance : 'a -> module_instance -> 'a
  method builtin_op      : 'a -> builtin_op -> 'a
  method user_defined_op : 'a -> user_defined_op -> 'a
  method expr_or_assume_prove : 'a -> expr_or_assume_prove -> 'a
  method proof           : 'a -> proof -> 'a
  method step            : 'a -> step -> 'a
  method instance        : 'a -> instance -> 'a
  method use_or_hide     : 'a -> use_or_hide -> 'a
  method subst           : 'a -> subst -> 'a
  method label           : 'a -> label -> 'a
  method let_in          : 'a -> let_in -> 'a
  method subst_in        : 'a -> subst_in -> 'a
  method node            : 'a -> node -> 'a
  method def_step        : 'a -> def_step -> 'a
    
  method expr_or_module_or_module_instance                       : 'a -> expr_or_module_or_module_instance -> 'a
  method user_defined_op_or_module_instance_or_theorem_or_assume : 'a -> user_defined_op_or_module_instance_or_theorem_or_assume -> 'a
  method new_symb_or_expr_or_assume_prove                        : 'a -> new_symb_or_expr_or_assume_prove -> 'a
  method op_def_or_theorem_or_assume                             : 'a -> op_def_or_theorem_or_assume -> 'a
    
end
 = object(self)
  method node acc = function
  | N_ap_subst_in x  -> self#ap_subst_in acc x
  | N_assume_prove x -> self#assume_prove acc x
  | N_def_step x     -> self#def_step acc x
  | N_expr x         -> self#expr acc x
  | N_op_arg x       -> self#op_arg acc x
  | N_instance x     -> self#instance acc x
  | N_new_symb x     -> self#new_symb acc x
  | N_proof x        -> self#proof acc x
  | N_formal_param x -> self#formal_param acc x
  | N_module x       -> self#mule acc x
  | N_op_decl x      -> self#op_decl acc x
  | N_op_def x       -> self#op_def acc x
  | N_assume x       -> self#assume acc x
  | N_theorem x      -> self#theorem acc x
  | N_use_or_hide x  -> self#use_or_hide acc x

  (* parts of expressions *)
   method location acc l : 'a = acc
   method level acc l : 'a = acc

  (* non-recursive expressions *) 
   method decimal acc d = acc
   method numeral acc n = acc
   method strng acc s = acc

(* recursive expressions *)
   method at acc0 {location; level; except; except_component} : 'a =
     let acc1 = self#location acc0 location in
     let acc2 = self#level acc1 level in
     let acc3 = self#op_appl acc2 except in
     let acc = self#op_appl acc3 except_component in
     acc

   method op_appl acc0 {location; level; operator; operands; bound_symbols} : 'a=
     let acc1 = self#location acc0 location in
     let acc2 = self#level acc1 level in
     let acc3 = self#fmota acc2 operator in
     let acc4 = List.fold_left self#expr_or_op_arg acc3 operands in
     let acc = List.fold_left self#bound_symbol acc4 bound_symbols in 
     acc

       
   method bound_symbol acc = function
   | B_bounded_bound_symbol s -> self#bounded_bound_symbol acc s
   | B_unbounded_bound_symbol s -> self#unbounded_bound_symbol acc s

   method bounded_bound_symbol acc x = acc
   method unbounded_bound_symbol acc x = acc


   method formal_param acc0 = function
   | FP_ref i -> acc0
   | FP { location; level; name; arity; } ->
     let acc1 = self#location acc0 location in
     let acc2 = self#level acc1 level in
     let acc3 = self#name acc2 name in
     (* arity skipped *)
     acc3
     
   method mule acc0 = function
   | MOD_ref i -> acc0
   | MOD {name; location; constants; variables; definitions; assumptions; theorems; } ->
     let acc1 = self#location acc0 location in
     let acc2 = List.fold_left self#op_decl acc1 constants in
     let acc3 = List.fold_left self#op_decl acc2 variables in
     let acc4 = List.fold_left self#op_def acc3 definitions in
     let acc5 = List.fold_left self#assume acc4 assumptions in
     let acc = List.fold_left self#theorem acc5 theorems in
     acc
     
   method op_arg acc0 {location; level; name; arity } =
     (* terminal node *)
     let acc1 = self#location acc0 location in
     let acc2 = self#level acc1 level in
   (*skip name and arity *)
     acc2

   method op_decl acc0 = function
   | OPD_ref x -> acc0
   | OPD  { location ; level ; name ; arity ; kind ; } ->
   (* terminal node *)
     let acc1 = self#location acc0 location in
     let acc2 = self#level acc1 level in
   (* skip name, arity and kind *)
     acc2

   method op_def acc = function
   | OPDef_ref x -> acc
   | OPDef (O_module_instance x) -> self#module_instance acc x
   | OPDef (O_builtin_op x)      -> self#builtin_op acc x
   | OPDef (O_user_defined_op x) -> self#user_defined_op acc x
       
   method theorem acc0 = function
   | THM_ref x -> acc0
   | THM { location; level; expr; proof; suffices } ->
     let acc1 = self#location acc0 location in
     let acc2 = self#level acc1 level in
     let acc3 = self#expr_or_assume_prove acc2 expr in
     let acc4 = self#proof acc3 proof  in
     (* todo: finish*)
     acc4
     
   method assume acc0  = function
   | ASSUME_ref x -> acc0
   | ASSUME {location; level; expr; } ->
     let acc1 = self#location acc0 location in
     let acc2 = self#level acc1 level in
     let acc = self#expr acc2 expr in
     acc
       
   method proof acc0 = function
   | P_omitted location -> acc0
   | P_obvious location -> acc0
   | P_by { location; level; facts; defs; only} ->
     let acc1 = self#location acc0 location in
     let acc2 = self#level acc1 level in
     let acc3 = List.fold_left self#expr_or_module_or_module_instance acc2 facts in
     let acc = List.fold_left self#user_defined_op_or_module_instance_or_theorem_or_assume acc3 defs in
     (* skip the only tag *)
     acc
   | P_steps { location; level; steps; } ->
     List.fold_left self#step acc0 steps
   | P_noproof -> acc0

   method step acc0 = function
   | S_def_step x -> self#def_step acc0 x
   | S_use_or_hide x -> self#use_or_hide acc0 x
   | S_instance i -> self#instance acc0 i
   | S_theorem t -> self#theorem acc0 t

   method use_or_hide acc0 {  location; level; facts; defs; only; hide } =
     let acc1 = self#location acc0 location in
     let acc2 = self#level acc1 level in
     let acc3 = List.fold_left self#expr_or_module_or_module_instance acc2 facts in
     let acc = List.fold_left self#user_defined_op_or_module_instance_or_theorem_or_assume acc3 defs in
     acc
     
   method instance acc0 {location; level; name; substs; params; } =
     let acc1 = self#location acc0 location in
     let acc2 = self#level acc1 level in
     let acc3 = self#name acc2 name in     
     let acc4 = List.fold_left self#subst acc3 substs in
     let acc = List.fold_left self#formal_param acc4 params in
     acc

   method subst acc0 { op; expr } =
     let acc1 = self#op_decl acc0 op in
     let acc = self#expr_or_op_arg acc1 expr in
     acc
       
   method assume_prove acc0 { location; level; assumes; prove; suffices; boxed; } =
     let acc1 = self#location acc0 location in
     let acc2 = self#level acc1 level in
     let acc3 = List.fold_left self#new_symb_or_expr_or_assume_prove acc2 assumes in
     let acc = self#expr acc3 prove in
     (* suffices and boxed are boolean flags*)
     acc

   method new_symb acc0 { location; level; op_decl; set } = 
     let acc1 = self#location acc0 location in
     let acc2 = self#level acc1 level in
     let acc3 = self#op_decl acc2 op_decl in
     let acc = match set with
       | None -> acc3
       | Some e -> self#expr acc3 e
     in acc

   method let_in acc0 {location; level; body; op_defs } =
     let acc1 = self#location acc0 location in
     let acc2 = self#level acc1 level in
     let acc3 = self#expr acc2 body in
     let acc = List.fold_left self#op_def_or_theorem_or_assume acc3 op_defs in
     acc
       
   method subst_in acc0 ({ location; level; substs; body } : subst_in) =
     let acc1 = self#location acc0 location in
     let acc2 = self#level acc1 level in
     let acc3 = List.fold_left self#subst acc2 substs in
     let acc = self#expr acc3 body in
     acc
     
   method label acc0 ({location; level; name; arity; body; params } : label) =
     let acc1 = self#location acc0 location in
     let acc2 = self#level acc1 level in
     let acc3 = self#name acc2 name in
     (* skip arity *)
     let acc4 = self#expr_or_assume_prove acc3 body in
     let acc = List.fold_left self#formal_param acc4 params in
     acc     
       
   method ap_subst_in acc0 ({ location; level; substs; body } : ap_subst_in) =
     let acc1 = self#location acc0 location in
     let acc2 = self#level acc1 level in
     let acc3 = List.fold_left self#subst acc2 substs in
     let acc = self#node acc3 body in
     acc

   method def_step acc0 { location; level; defs } =
     let acc1 = self#location acc0 location in
     let acc2 = self#level acc1 level in
     let acc = List.fold_left self#op_def acc2 defs in
     acc
       
   method module_instance acc0 = function
   | MI_ref x -> acc0
   | MI {location; level; name} ->
     let acc1 = self#location acc0 location in
     let acc2 = self#level acc1 level in
     let acc = self#name acc2 name in
     acc
     
   method builtin_op acc = function
   | BOP_ref x -> acc
   | BOP {location; level; name; arity; params } ->
     let acc1 = self#location acc location in
     let acc2 = self#level acc1 level in
     let acc3 = self#name acc2 name in
   (* skip arity *)
     let acc = List.fold_left (fun x (fp,_) -> self#formal_param x fp) acc3 params
     in acc
     
   method user_defined_op acc0 = function
   | UOP_ref x -> acc0
   | UOP { location; level ; name ; arity ;
	   body ; params ; recursive ; } ->
     let acc1 = self#location acc0 location in
     let acc2 = self#level acc1 level in
     let acc3 = self#name acc2 name in
   (* arity *)
     let acc4 = self#expr acc3 body in
     let acc = List.fold_left (fun x (fp,_) -> self#formal_param x fp) acc4 params in
   (* skip recursive flag *)
     acc

   method name acc x = acc

   (* pure disjunction types *)
   method expr acc = function
   | E_at x        -> self#at acc x
   | E_decimal x   -> self#decimal acc x
   | E_label x     -> self#label acc x
   | E_let_in x    -> self#let_in acc x
   | E_numeral x   -> self#numeral acc x
   | E_op_appl x   -> self#op_appl acc x
   | E_string x    -> self#strng acc x
   | E_subst_in x  -> self#subst_in acc x

   method expr_or_module_or_module_instance acc = function
   | EMM_expr x            -> self#expr acc x
   | EMM_module_instance x -> self#module_instance acc x
   | EMM_module x          -> self#mule acc x
     
   method user_defined_op_or_module_instance_or_theorem_or_assume acc = function
   | UMTA_user_defined_op x -> self#user_defined_op acc x
   | UMTA_module_instance x -> self#module_instance acc x
   | UMTA_theorem x         -> self#theorem acc x
   | UMTA_assume x          -> self#assume acc x

     
   method new_symb_or_expr_or_assume_prove acc = function
   | NEA_new_symb s      -> self#new_symb acc s
   | NEA_expr e          -> self#expr acc e
   | NEA_assume_prove ap -> self#assume_prove acc ap

   method op_def_or_theorem_or_assume acc = function
   | OTA_op_def x -> self#op_def acc x
   | OTA_theorem x -> self#theorem acc x
   | OTA_assume x -> self#assume acc x

   method expr_or_assume_prove acc = function
   | EA_assume_prove ap -> self#assume_prove acc ap
   | EA_expr e          -> self#expr acc e

   method expr_or_op_arg acc = function
   | EO_op_arg oa -> self#op_arg acc oa
   | EO_expr e -> self#expr acc e

     
   method fmota acc = function
   | FMOTA_formal_param x -> self#formal_param acc x
   | FMOTA_module  x -> self#mule acc x
   | FMOTA_op_decl x -> self#op_decl acc x
   | FMOTA_op_def  x -> self#op_def acc x
   | FMOTA_theorem x -> self#theorem acc x
   | FMOTA_assume  x -> self#assume acc x
   | FMOTA_ap_subst_in x -> self#ap_subst_in acc x
 end
  
