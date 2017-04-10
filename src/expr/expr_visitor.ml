open Commons
open Expr_ds

class ['a] visitor :
  object
    method expr            : 'a -> expr -> 'a
    method name            : 'a -> string -> 'a
    method location        : 'a -> location -> 'a
    method level           : 'a -> level option -> 'a
    method decimal         : 'a -> decimal -> 'a
    method numeral         : 'a -> numeral -> 'a
    method strng           : 'a -> strng -> 'a
    method at              : 'a -> at -> 'a
    method op_appl         : 'a -> op_appl -> 'a
    method binder          : 'a -> binder -> 'a
    method lambda          : 'a -> lambda -> 'a
    method op_arg          : 'a -> op_arg -> 'a
    method operator        : 'a -> operator -> 'a
    method expr_or_op_arg  : 'a -> expr_or_op_arg -> 'a
    method bound_symbol    : 'a -> bound_symbol -> 'a
    method bounded_bound_symbol   : 'a -> bounded_bound_symbol -> 'a
    method unbounded_bound_symbol : 'a -> unbounded_bound_symbol -> 'a
    method mule            : 'a -> mule -> 'a
    method mule_entry      : 'a -> mule_entry -> 'a
    method formal_param    : 'a -> formal_param -> 'a
    method op_decl         : 'a -> op_decl -> 'a
    method op_def          : 'a -> op_def -> 'a
    method theorem         : 'a -> theorem -> 'a
    method theorem_def     : 'a -> theorem_def -> 'a
    method assume_def      : 'a -> assume_def -> 'a
    method statement       : 'a -> statement -> 'a
    method assume          : 'a -> assume -> 'a
    method assume_prove    : 'a -> assume_prove -> 'a
    method new_symb        : 'a -> new_symb -> 'a
    method ap_subst_in     : 'a -> ap_subst_in -> 'a
    method module_instance : 'a -> module_instance -> 'a
    method builtin_op      : 'a -> builtin_op -> 'a
    method user_defined_op : 'a -> user_defined_op -> 'a
    method proof           : 'a -> proof -> 'a
    method step            : 'a -> step -> 'a
    method instance        : 'a -> instance -> 'a
    method use_or_hide     : 'a -> use_or_hide -> 'a
    method instantiation   : 'a -> instantiation -> 'a
    (*  method subst           : 'a -> subst -> 'a *)
    method label           : 'a -> label -> 'a
    method let_in          : 'a -> let_in -> 'a
    method subst_in        : 'a -> subst_in -> 'a
    method node            : 'a -> node -> 'a
    method def_step        : 'a -> def_step -> 'a

    method formal_param_   : 'a -> formal_param_ -> 'a
    method mule_           : 'a -> mule_ -> 'a
    method op_decl_        : 'a -> op_decl_ -> 'a
    method module_instance_  : 'a -> module_instance_ -> 'a
    method user_defined_op_  : 'a -> user_defined_op_ -> 'a
    method builtin_op_     : 'a -> builtin_op_ -> 'a
    method theorem_def_    : 'a -> theorem_def_ -> 'a
    method assume_def_     : 'a -> assume_def_ -> 'a
    method theorem_        : 'a -> theorem_ -> 'a
    method assume_         : 'a -> assume_ -> 'a
    method theorem_def_    : 'a -> theorem_def_ -> 'a
    method assume_def_     : 'a -> assume_def_ -> 'a

    method reference       : 'a -> int -> 'a

    method entry           : 'a -> (int * entry) -> 'a
    method context         : 'a -> context -> 'a

    method op_appl_or_binder : 'a -> op_appl_or_binder -> 'a
    method expr_or_module_or_module_instance :
      'a -> expr_or_module_or_module_instance -> 'a

    method defined_expr : 'a -> defined_expr -> 'a

    method op_def_or_theorem_or_assume       :
      'a -> op_def_or_theorem_or_assume -> 'a

  end
  = object(self)

    method node acc = function
      | N_ap_subst_in x  -> self#ap_subst_in acc x
      | N_assume_prove x -> self#assume_prove acc x
      | N_expr x         -> self#expr acc x

    (* parts of expressions *)
    method location acc l : 'a = acc
    method level acc l : 'a = acc

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
      acc

    method op_appl acc0 ({location; level; operator; operands} : op_appl) =
      let acc1 = self#location acc0 location in
      let acc2 = self#level acc1 level in
      let acc3 = self#operator acc2 operator in
      let acc = List.fold_left self#expr_or_op_arg acc3 operands in
      acc

    method binder acc0 { location; level; operator; operand;
                         bound_symbols } =
      let acc1 = self#location acc0 location in
      let acc2 = self#level acc1 level in
      let acc3 = self#operator acc2 operator in
      let acc4 = self#expr_or_op_arg acc3 operand in
      let acc = List.fold_left self#bound_symbol acc4 bound_symbols in
      acc

    method lambda acc ({level; arity; body; params; } : lambda) =
      let acc1 = self#level acc level in
      (* arity *)
      let acc2 = self#expr acc1 body in
      let acc = List.fold_left
          (fun x (fp,_) -> self#formal_param x fp) acc2 params in
      acc

    method bound_symbol acc = function
      | B_bounded_bound_symbol s -> self#bounded_bound_symbol acc s
      | B_unbounded_bound_symbol s -> self#unbounded_bound_symbol acc s

    method bounded_bound_symbol acc0 { params; tuple; domain } =
      let acc1 = List.fold_left self#formal_param acc0 params in
      (* skip tuple *)
      let acc2 = self#expr acc1 domain in
      acc2

    method unbounded_bound_symbol acc0 { param; tuple; } =
      let acc1 = self#formal_param acc0 param in
      acc1


    method formal_param acc0 (FP_ref i) =
      self#reference acc0 i

    method formal_param_ acc0
        ({ id; location; level; name; arity; } :formal_param_) =
      let acc1 = self#location acc0 location in
      let acc2 = self#level acc1 level in
      let acc3 = self#name acc2 name in
      (* arity skipped *)
      acc3

    method mule acc0 (MOD_ref i) =
      self#reference acc0 i

    method mule_ acc {name; location; module_entries } =
      let acc0a = self#name acc name in
      let acc1 = self#location acc0a location in
      let acc = List.fold_left self#mule_entry acc1 module_entries in
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

    method op_decl_ acc0 { location ; level ; name ; arity ; kind ; } =
      (* terminal node *)
      let acc1 = self#location acc0 location in
      let acc2 = self#level acc1 level in
      let acc3 = self#name acc2 name in
      (* skip arity and kind *)
      acc3

    method op_def acc = function
      | O_module_instance x -> self#module_instance acc x
      | O_builtin_op x      -> self#builtin_op acc x
      | O_user_defined_op x -> self#user_defined_op acc x
      | O_thm_def x         -> self#theorem_def acc x
      | O_assume_def x      -> self#assume_def acc x

    method theorem acc0 = function
      | THM_ref x -> self#reference acc0 x

    method theorem_ acc0 { id; location; level; definition; statement; proof; } =
        let acc1 = self#location acc0 location in
        let acc2 = self#level acc1 level in
        let acc3 = match definition with
          | None -> acc2
          | Some d -> self#theorem_def acc2 d in
        let acc4 = self#statement acc3 statement in
        let acc5 = self#proof acc4 proof  in
        acc5

    method statement acc0 = function
      | ST_FORMULA f -> self#node acc0 f
      | ST_SUFFICES f -> self#node acc0 f
      | ST_CASE f -> self#expr acc0 f
      | ST_PICK {variables; formula} ->
        let acc1 = List.fold_left self#bound_symbol acc0 variables in
        let acc2 = self#expr acc1 formula in
        acc2
      | ST_QED  -> acc0
      | ST_HAVE expr -> self#expr acc0 expr
      | ST_TAKE bound_symbols ->
        List.fold_left self#bound_symbol acc0 bound_symbols
      | ST_WITNESS expr -> self#expr acc0 expr

    method assume acc0 (ASSUME_ref x) =
      self#reference acc0 x

    method assume_ acc0 {location; level; expr; } =
      let acc1 = self#location acc0 location in
      let acc2 = self#level acc1 level in
      let acc = self#expr acc2 expr in
      acc

    method theorem_def acc0 = function
      | TDef_ref x -> self#reference acc0 x

    method assume_def acc0 = function
      | ADef_ref x -> self#reference acc0 x

    method assume_def_ acc0 (a:assume_def_) = match a with
      | {location; level; name; body }  ->
       let acc1 = self#location acc0 location in
       let acc2 = self#level acc1 level in
       let acc3 = self#name acc2 name in
       let acc4 = self#expr acc3 body in
       acc4

    method theorem_def_ acc0 (td:theorem_def_) =
      match td with
      | {location; level; name; body } ->
        let acc1 = self#location acc0 location in
        let acc2 = self#level acc1 level in
        let acc3 = self#name acc2 name in
        let acc4 = self#node acc3 body in
        acc4

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

    method instance acc0 {location; level; name; module_name; substs; params; } =
      let acc1 = self#location acc0 location in
      let acc2 = self#level acc1 level in
      let acc3 = match name with
        | None -> acc2
        | Some name -> self#name acc2 name
      in
      let acc4 = self#name acc3 module_name in
      let acc5 = List.fold_left self#instantiation acc4 substs in
      let acc = List.fold_left self#formal_param acc5 params in
      acc

    method instantiation acc0 { op; expr } =
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
          self#node acc3 assumes in
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
      let acc3 = List.fold_left self#instantiation acc2 substs in
      let acc = self#expr acc3 body in
      acc

    method label acc0 ({location; level; name; arity; body; params } : label) =
      let acc1 = self#location acc0 location in
      let acc2 = self#level acc1 level in
      let acc3 = self#name acc2 name in
      (* skip arity *)
      let acc4 = self#node acc3 body in
      let acc = List.fold_left self#formal_param acc4 params in
      acc

    method ap_subst_in acc0 ({ location; level; substs; body } : ap_subst_in) =
      let acc1 = self#location acc0 location in
      let acc2 = self#level acc1 level in
      let acc3 = List.fold_left self#instantiation acc2 substs in
      let acc = self#node acc3 body in
      acc

    method def_step acc0 { location; level; defs } =
      let acc1 = self#location acc0 location in
      let acc2 = self#level acc1 level in
      let acc = List.fold_left self#op_def acc2 defs in
      acc

    method module_instance acc0 = function
      | MI_ref x -> self#reference acc0 x

    method module_instance_ acc0
        ({id; location; level; name} : module_instance_) =
      let acc1 = self#location acc0 location in
      let acc2 = self#level acc1 level in
      let acc = self#name acc2 name in
      acc

    method builtin_op acc0 (BOP_ref r) =
      self#reference acc0 r

    method builtin_op_ acc0 { id; level; name; arity; params } =
      let acc1 = self#level acc0 level in
      let acc2 = self#name acc1 name in
      (* skip arity *)
      let acc = List.fold_left
          (fun x (fp,_) -> self#formal_param x fp) acc2 params
      in acc

    method user_defined_op acc0 = function
      | UOP_ref x -> self#reference acc0 x

    method user_defined_op_ acc0
        { id; location; level ; name ; arity ; body ; params ;  } =
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
      | FP_entry x -> self#formal_param_ acc x
      | BI_entry x -> self#builtin_op_ acc x
      | MOD_entry x -> self#mule_ acc x
      | OPDec_entry x -> self#op_decl_ acc x
      | MI_entry x -> self#module_instance_ acc x
      | UOP_entry x -> self#user_defined_op_ acc x
      | BOP_entry x -> self#builtin_op_ acc x
      | TDef_entry x -> self#theorem_def_ acc x
      | ADef_entry x -> self#assume_def_ acc x
      | THM_entry x -> self#theorem_ acc x
      | ASSUME_entry x -> self#assume_ acc x

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
      | UMTA_theorem_def x     -> self#theorem_def acc x
      | UMTA_assume_def x      -> self#assume_def acc x

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
      | FMOTA_lambda x    -> self#lambda acc x

    method mule_entry acc = function
      | MODe_op_decl x     -> self#op_decl acc x
      | MODe_op_def x      -> self#op_def acc x
      | MODe_assume x      -> self#assume acc x
      | MODe_theorem x     -> self#theorem acc x
      | MODe_instance x    -> self#instance acc x
      | MODe_use_or_hide x -> self#use_or_hide acc x
  end
