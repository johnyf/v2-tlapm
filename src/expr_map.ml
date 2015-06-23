open Commons
open Expr_ds
open Expr_visitor

(** A transformer template for expressions.
 *)

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



let get_anyexpr (x, _) = x
let update_anyexpr (_, acc) (any, _) = (acc, any)

let  extract_ap_subst_in acc =
  match get_anyexpr acc with Any_ap_subst_in x -> x
let  extract_assume acc =
  match get_anyexpr acc with Any_assume x -> x
let  extract_assume_prove acc =
  match get_anyexpr acc with Any_assume_prove x -> x
let  extract_at acc =
  match get_anyexpr acc with Any_at x -> x
let  extract_bounded_bound_symbol acc =
  match get_anyexpr acc with Any_bounded_bound_symbol x -> x
let  extract_builtin_op acc =
  match get_anyexpr acc with Any_builtin_op x -> x
let  extract_decimal acc =
  match get_anyexpr acc with Any_decimal x -> x
let  extract_def_step acc =
  match get_anyexpr acc with Any_def_step x -> x
let  extract_expr acc =
  match get_anyexpr acc with Any_expr x -> x
let  extract_expr_or_oparg acc =
  match get_anyexpr acc with Any_expr_or_oparg x -> x
let  extract_formal_param acc =
  match get_anyexpr acc with Any_formal_param x -> x
let  extract_instance acc =
  match get_anyexpr acc with Any_instance x -> x
let  extract_label acc =
  match get_anyexpr acc with Any_label x -> x
let  extract_let_in acc =
  match get_anyexpr acc with Any_let_in x -> x
let  extract_level acc =
  match get_anyexpr acc with Any_level x -> x
let  extract_level_option acc =
  match get_anyexpr acc with
  | Some (Any_level x) -> Some x
  | None -> None
let  extract_location acc =
  match get_anyexpr acc with Any_location x -> x
let  extract_module_instance acc =
  match get_anyexpr acc with Any_module_instance x -> x
let  extract_mule acc =
  match get_anyexpr acc with Any_mule x -> x
let  extract_node acc =
  match get_anyexpr acc with Any_node x -> x
let  extract_numeral acc =
  match get_anyexpr acc with Any_numeral x -> x
let  extract_op_arg acc =
  match get_anyexpr acc with Any_op_arg x -> x
let  extract_op_decl acc =
  match get_anyexpr acc with Any_op_decl x -> x
let  extract_op_def acc =
  match get_anyexpr acc with Any_op_def x -> x
let  extract_operator acc =
  match get_anyexpr acc with Any_operator x -> x
let  extract_proof acc =
  match get_anyexpr acc with Any_proof x -> x
let  extract_strng acc =
  match get_anyexpr acc with Any_strng x -> x
let  extract_subst_in acc =
  match get_anyexpr acc with Any_subst_in x -> x
let  extract_theorem acc =
  match get_anyexpr acc with Any_theorem x -> x
let  extract_unbounded_bound_symbol acc =
  match get_anyexpr acc with Any_unbounded_bound_symbol x -> x
let  extract_use_or_hid acc =
  match get_anyexpr acc with Any_use_or_hide x -> x
let  extract_user_defined_op acc =
  match get_anyexpr acc with Any_user_defined_op x -> x

type ('a, 'b) macc =  'a anyExpr * 'b

class ['a,'b] expr_map = object(self)
inherit [('a, 'b) macc] visitor as super

  (* parts of expressions *)
   method location acc l  = acc
   method level acc l  = acc

  (* non-recursive expressions *)
   method decimal acc d = acc
   method numeral acc n = acc
   method strng acc s = acc

(* recursive expressions *)
   method at acc0 {location; level; except; except_component} =
     let acc1 = self#location acc0 location in
     let acc2 = self#level acc1 level in
     let acc3 = self#op_appl_or_binder acc2 except in
     let acc = self#op_appl_or_binder acc3 except_component in
(* TODO: the type 'a is set for the whole class, so using self#location and
         self#level unifies the type of the passed result.
     let r = {
     location = extract_location acc1;
     level = extract_level_option acc2;
     except = extract_op_appl_or_binder acc3;
     except_component = extract_op_appl_or_binder acc4;
     } in
 *)
     acc

   method op_appl acc0 ({location; level; operator; operands} : op_appl) =
     let acc1 = self#location acc0 location in
     let acc2 = self#level acc1 level in
     let acc3 = self#operator acc2 operator in
     let acc = List.fold_left self#expr_or_op_arg acc3 operands in
     acc

   method binder acc0 {location; level; operator; operand; bound_symbols} =
     let acc1 = self#location acc0 location in
     let acc2 = self#level acc1 level in
     let acc3 = self#operator acc2 operator in
     let acc4 = self#expr_or_op_arg acc3 operand in
     let acc = List.fold_left self#bound_symbol acc4 bound_symbols in
     acc

   method bound_symbol acc = function
   | B_bounded_bound_symbol s -> self#bounded_bound_symbol acc s
   | B_unbounded_bound_symbol s -> self#unbounded_bound_symbol acc s

   method bounded_bound_symbol acc x = acc
   method unbounded_bound_symbol acc x = acc


   method formal_param acc0 = function
   | FP_ref i -> self#reference acc0 i
   | FP { location; level; name; arity; } ->
     let acc1 = self#location acc0 location in
     let acc2 = self#level acc1 level in
     let acc3 = self#name acc2 name in
     (* arity skipped *)
     acc3

   method mule acc0 = function
   | MOD_ref i -> self#reference acc0 i
   | MOD {name; location; constants; variables;
	  definitions; assumptions; theorems; } ->
     let acc0a = self#name acc0 name in
     let acc1 = self#location acc0a location in
     let acc2 = List.fold_left self#op_decl acc1 constants in
     let acc3 = List.fold_left self#op_decl acc2 variables in
     let acc4 = List.fold_left self#op_def acc3 definitions in
     let acc5 = List.fold_left self#assume acc4 assumptions in
     let acc = List.fold_left self#theorem acc5 theorems in
     acc

   method op_arg acc0 {location; level; argument } =
     (* terminal node *)
     let acc1 = self#location acc0 location in
     let acc2 = self#level acc1 level in
   (*skip arity *)
     let acc3 = self#operator acc2 argument in
     acc3

   method op_decl acc0 = function
   | OPD_ref x -> self#reference acc0 x
   | OPD  { location ; level ; name ; arity ; kind ; } ->
   (* terminal node *)
     let acc1 = self#location acc0 location in
     let acc2 = self#level acc1 level in
     let acc3 = self#name acc2 name in
   (* skip arity and kind *)
     acc3

   method op_def acc = function
   | OPDef_ref x -> self#reference acc x
   | OPDef (O_module_instance x) -> self#module_instance acc x
   | OPDef (O_builtin_op x)      -> self#builtin_op acc x
   | OPDef (O_user_defined_op x) -> self#user_defined_op acc x

   method theorem acc0 = function
   | THM_ref x -> self#reference acc0 x
   | THM { location; level; name; expr; proof; suffices } ->
     let acc1 = self#location acc0 location in
     let acc2 = self#level acc1 level in
     let acc2a = match name with
       | None -> acc2
       | Some n -> self#name acc2 n in
     let acc3 = self#assume_prove acc2a expr in
     let acc4 = self#proof acc3 proof  in
     (* skip suffices *)
     acc4

   method assume acc0  = function
   | ASSUME_ref x -> self#reference acc0 x
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
     let acc3 = List.fold_left
       self#expr_or_module_or_module_instance acc2 facts in
     let acc = List.fold_left
       self#defined_expr acc3 defs in
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
     let acc3 = List.fold_left
       self#expr_or_module_or_module_instance acc2 facts in
     let acc = List.fold_left
       self#defined_expr acc3 defs in
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

   method assume_prove acc0 { location; level; new_symbols; assumes;
			      prove; suffices; boxed; } =
     let acc1 = self#location acc0 location in
     let acc2 = self#level acc1 level in
     let acc3 = List.fold_left
		  self#new_symb acc2 new_symbols in
     let acc4 = List.fold_left
		  self#assume_prove acc3 assumes in
     let acc = self#expr acc4 prove in
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
     let acc4 = self#assume_prove acc3 body in
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
   | MI_ref x -> self#reference acc0 x
   | MI {location; level; name} ->
     let acc1 = self#location acc0 location in
     let acc2 = self#level acc1 level in
     let acc = self#name acc2 name in
     acc

   method builtin_op acc0 = function
   | { level; name; arity; params } ->
     let acc1 = self#level acc0 level in
     let acc2 = self#name acc1 name in
   (* skip arity *)
     let acc = List.fold_left
       (fun x (fp,_) -> self#formal_param x fp) acc2 params
     in acc

   method user_defined_op acc0 = function
   | UOP_ref x -> self#reference acc0 x
   | UOP { location; level ; name ; arity ;
           body ; params ; recursive ; } ->
     let acc1 = self#location acc0 location in
     let acc2 = self#level acc1 level in
     let acc3 = self#name acc2 name in
   (* arity *)
     let acc4 = self#expr acc3 body in
     let acc = List.fold_left
       (fun x (fp,_) -> self#formal_param x fp) acc4 params in
   (* skip recursive flag *)
     acc

   method name acc x = acc

   method reference acc x = acc

   method entry acc (id, e) = match e with
   | FP_entry x -> self#formal_param acc (FP x)
   | MOD_entry x -> self#mule acc (MOD x)
   | OPDef_entry x -> self#op_def acc (OPDef x)
   | OPDec_entry x -> self#op_decl acc (OPD x)
   | THM_entry x -> self#theorem acc (THM x)
   | ASSUME_entry x -> self#assume acc (ASSUME x)
   | APSUBST_entry x -> self#ap_subst_in acc x

   method context acc {   entries; modules } =
     let acc1 = List.fold_left self#entry acc entries in
     let acc2 = List.fold_left self#mule acc1 modules in
     acc2

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
   | E_binder x    -> self#binder acc x

   method op_appl_or_binder acc0 = function
     | OB_op_appl x -> self#op_appl acc0 x
     | OB_binder x -> self#binder acc0 x

   method expr_or_module_or_module_instance acc = function
   | EMM_expr x            -> self#expr acc x
   | EMM_module_instance x -> self#module_instance acc x
   | EMM_module x          -> self#mule acc x

   method defined_expr acc = function
   | UMTA_user_defined_op x -> self#user_defined_op acc x
   | UMTA_module_instance x -> self#module_instance acc x
   | UMTA_theorem x         -> self#theorem acc x
   | UMTA_assume x          -> self#assume acc x

   method op_def_or_theorem_or_assume acc = function
   | OTA_op_def x -> self#op_def acc x
   | OTA_theorem x -> self#theorem acc x
   | OTA_assume x -> self#assume acc x

   method expr_or_op_arg acc = function
   | EO_op_arg oa -> self#op_arg acc oa
   | EO_expr e -> self#expr acc e

   method operator acc = function
   | FMOTA_formal_param x -> self#formal_param acc x
   | FMOTA_module  x -> self#mule acc x
   | FMOTA_op_decl x -> self#op_decl acc x
   | FMOTA_op_def  x -> self#op_def acc x
   | FMOTA_theorem x -> self#theorem acc x
   | FMOTA_assume  x -> self#assume acc x
   | FMOTA_ap_subst_in x -> self#ap_subst_in acc x

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

end
