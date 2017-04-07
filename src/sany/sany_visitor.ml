open Commons
open Sany_ds

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
    method mule_entry      : 'a -> mule_entry -> 'a
    method formal_param    : 'a -> formal_param -> 'a
    method op_decl         : 'a -> op_decl -> 'a
    method op_def          : 'a -> op_def -> 'a
    method builtin_op      : 'a -> builtin_op -> 'a
    method theorem_def     : 'a -> theorem_def -> 'a
    method theorem         : 'a -> theorem -> 'a
    method assume_def      : 'a -> assume_def -> 'a
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
    method reference       : 'a -> int -> 'a

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

    method context         : 'a -> context -> 'a
    method entry           : 'a -> entry -> 'a

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
    method at acc0 {location; level; except; except_component} =
      let acc1 = self#location acc0 location in
      let acc2 = self#level acc1 level in
      let acc3 = self#op_appl acc2 except in
      let acc = self#op_appl acc3 except_component in
      acc

    method op_appl acc0 {location; level; operator; operands; bound_symbols} =
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
      | FP_ref i -> self#reference acc0 i
      | FP fp -> self#formal_param_ acc0 fp

    method formal_param_ acc0 = function
      | { location; level; name; arity; } ->
        let acc1 = self#location acc0 location in
        let acc2 = self#level acc1 level in
        let acc3 = self#name acc2 name in
        (* arity skipped *)
        acc3

    method mule acc0 = function
      | MOD_ref i -> self#reference acc0 i

    method mule_ acc0 = function
      | {name; location; module_entries } ->
        let acc0a = self#name acc0 name in
        let acc1 = self#location acc0a location in
        let acc = List.fold_left self#mule_entry acc1 module_entries in
        acc

    method op_arg acc0 {location; level; argument } =
      (* terminal node *)
      let acc1 = self#location acc0 location in
      let acc2 = self#level acc1 level in
      (*skip arity *)
      let acc3 = self#fmota acc2 argument in
      acc3

    method op_decl acc0 = function
      | OPD_ref x -> self#reference acc0 x

    method op_decl_ acc0 = function
      | { location ; level ; name ; arity ; kind ; } ->
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

    method theorem_def acc0 = function
      | TDef_ref x -> self#reference acc0 x

    method assume_def acc0 = function
      | ADef_ref x -> self#reference acc0 x

    method assume_def_ acc0 = function
      | {location; level; name; body } ->
        let acc1 = self#location acc0 location in
        let acc2 = self#level acc1 level in
        let acc3 = self#name acc2 name in
        let acc4 = self#expr_or_assume_prove in
        acc4
    
    method theorem acc0 = function
      | THM_ref x -> self#reference acc0 x

    method theorem_ acc0 = function
      | { location; level; definition; expr; proof; suffices } ->
        let acc1 = self#location acc0 location in
        let acc2 = self#level acc1 level in
        let acc2a = match definition with
          | None -> acc2
          | Some n -> self#theorem_def acc2 n in
        let acc3 = self#expr_or_assume_prove acc2a expr in
        let acc4 = self#proof acc3 proof  in
        (* skip suffices *)
        acc4

    method assume acc0  = function
      | ASSUME_ref x -> self#reference acc0 x

    method assume_ acc0  = function
      |  {location; level; expr; } ->
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
            self#user_defined_op_or_module_instance_or_theorem_or_assume acc3 defs in
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
          self#user_defined_op_or_module_instance_or_theorem_or_assume acc3 defs in
      acc

    method instance acc0 {location; level; name; module_name; substs; params; } =
      let acc1 = self#location acc0 location in
      let acc2 = self#level acc1 level in
      let acc3 = match name with
        | None -> acc2
        | Some name -> self#name acc2 name
      in
      let acc4 = self#name acc3 module_name in
      let acc5 = List.fold_left self#subst acc4 substs in
      let acc = List.fold_left self#formal_param acc5 params in
      acc

    method subst acc0 { op; expr } =
      let acc1 = self#op_decl acc0 op in
      let acc = self#expr_or_op_arg acc1 expr in
      acc

    method assume_prove acc0 { location; level; assumes;
                               prove; suffices; boxed; } =
      let acc1 = self#location acc0 location in
      let acc2 = self#level acc1 level in
      let acc3 = List.fold_left
          self#new_symb_or_expr_or_assume_prove acc2 assumes in
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
      | MI_ref x -> self#reference acc0 x

    method module_instance_ acc0 = function
      | {location; level; name} ->
        let acc1 = self#location acc0 location in
        let acc2 = self#level acc1 level in
        let acc = self#name acc2 name in
        acc

    method builtin_op acc0 = function
      | BOP_ref x -> self#reference acc0 x

    method builtin_op_ acc0 = function
      | {location; level; name; arity; params } ->
        let acc1 = self#location acc0 location in
        let acc2 = self#level acc1 level in
        let acc3 = self#name acc2 name in
        (* skip arity *)
        let acc = List.fold_left
            (fun x (fp,_) -> self#formal_param x fp) acc3 params
        in acc

    method user_defined_op acc0 = function
      | UOP_ref x -> self#reference acc0 x

    method user_defined_op_ acc0 = function
      | { location; level ; name ; arity ;
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

    method context acc { entries; modules } =
      let acc0 = List.fold_left self#entry acc entries in
      let acc1 = List.fold_left self#mule acc0 modules in
      acc1

    method entry acc { uid; reference } =
      (* skipping uid *)
      self#fmota acc reference

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

    method mule_entry acc = function
      | MODe_op_decl x     -> self#op_decl acc x
      | MODe_op_def x      -> self#op_def acc x
      | MODe_assume x      -> self#assume acc x
      | MODe_theorem x     -> self#theorem acc x
      | MODe_instance x    -> self#instance acc x
      | MODe_use_or_hide x -> self#use_or_hide acc x
  end
