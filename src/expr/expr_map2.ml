open Commons
open Expr_ds
open Expr_visitor
open Any_expr
open Util

type ('a, 'b) macc =  'a * 'b

let fold f acc l =
  List.fold_left (function (rs,acc) -> fun x ->
      let r, acc_ = f acc x in (r::rs, acc_)) ([], acc) l

let return acc x = (x, acc)

let opt_map f acc = function
  | Some x -> let r, acc_ = f acc x in (Some r, acc_)
  | None -> (None, acc)

class ['a] expr_map = object(self)
  method name (acc:'a) x =
    return acc x

  method reference (acc:'a) x =
    return acc x

  (* parts of expressions *)
  method location (acc:'a) (l:location)  =
    return acc l
  method level (acc:'a) (l:level)  =
    return acc l

  (* non-recursive expressions *)
  method decimal (acc:'a) d =
    return (acc:'a) d
  method numeral (acc:'a) n =
    return acc n
  method strng (acc:'a) s =
    return acc s
  (* recursive expressions *)
  method at (acc0:'a) {location; level; except; except_component} =
    let loaction, acc1 = self#location acc0 location in
    let level, acc2 = opt_map self#level acc1 level in
    let except, acc3 = self#op_appl_or_binder acc2 except in
    let except_component, acc4 = self#op_appl_or_binder acc3 except_component in
    let r = {  location; level; except; except_component; } in
    return acc4 r

  method op_appl acc0 ({location; level; operator; operands} : op_appl) =
    let location, acc1 = self#location acc0 location in
    let level, acc2 = opt_map self#level acc1 level in
    let operator, acc3 = self#operator acc2 operator in
    let operands, acc = fold self#expr_or_op_arg acc3 operands in
    let r =  { location; level; operator; operands; } in
    return acc r


  method binder acc0 {location; level; operator; operand; bound_symbols} =
    let location, acc1 = self#location acc0 location in
    let level, acc2 = opt_map self#level acc1 level in
    let operator, acc3 = self#operator acc2 operator in
    let operand, acc4 = self#expr_or_op_arg acc3 operand in
    let bound_symbols, acc = fold self#bound_symbol acc4 bound_symbols in
    let r = { location; level; operator; operand; bound_symbols; } in
    return acc r

  method lambda acc ({location; level; arity; body; params; } : lambda) =
    let location, acc0 = self#location acc location in
    let level, acc1 = opt_map self#level acc0 level in
    (* arity *)
    let body, acc2 = self#expr acc1 body in
    let params, acc = fold
        (fun x (fp,leibniz) ->
           let fp_, acc_ = self#formal_param x fp in
          ((fp_,leibniz), acc_))
        acc2 params in
    let r = { location; level; arity; body; params; } in
    return acc r

  method bound_symbol acc = function
    | B_bounded_bound_symbol s ->
      let any, acc1 = self#bounded_bound_symbol acc s in
      let bs = B_bounded_bound_symbol any in
      return acc1 bs
    | B_unbounded_bound_symbol s ->
      let any, acc1 = self#unbounded_bound_symbol acc s in
      let ubs = B_unbounded_bound_symbol any in
      return acc1 ubs

  method bounded_bound_symbol acc { params; tuple; domain } =
    let params, acc0 =
      fold self#formal_param acc params in
    let domain, acc1 = self#expr acc domain in
    let r =  { params; tuple; domain;} in
    return acc1 r

  method unbounded_bound_symbol acc { param; tuple}  =
    let param, acc0 = self#formal_param acc param in
    let r = { param; tuple; } in
    return acc0 r

  method formal_param acc0 = function
    | FP_ref i ->
      let fpr, acc1 = self#reference acc0 i in
      let r = FP_ref fpr in
      return acc1 r

  method formal_param_ acc0 ({ id; location; level; name; arity; } : formal_param_) =
      let location, acc1 = self#location acc0 location in
      let level, acc2 = opt_map self#level acc1 level in
      let name, acc3 = self#name acc2 name in
      (* arity skipped *)
      let r = ({ id; location; level; name; arity; } : formal_param_) in
      return acc3 r

  method mule acc0 = function
    | MOD_ref i ->
      let mr, acc1 = self#reference acc0 i in
      let r = MOD_ref mr in
      return acc1 r

  method mule_ acc0 {id; name; location; module_entries } =
      let name, acc1 = self#name acc0 name in
      let location, acc2 = self#location acc1 location in
      let module_entries,   acc =
        fold self#mule_entry acc2 module_entries in
      let r = {id; name; location; module_entries;} in
      return acc r

  method op_arg acc0 {location; level; argument } =
    (* terminal node *)
    let location, acc1 = self#location acc0 location in
    let level, acc2 = opt_map self#level acc1 level in
    let argument, acc3 = self#operator acc2 argument in
    let r =  { location; level; argument; } in
    return acc3 r

  method op_decl acc0 = function
    | OPD_ref x ->
      let reference, acc1 = self#reference acc0 x in
      let r = OPD_ref reference in
      return acc1 r

  method op_decl_ acc0 { id; location; level; name; arity; kind; } =
      (* terminal node *)
      let location, acc1 = self#location acc0 location in
      let level, acc2 = opt_map self#level acc1 level in
      let name, acc3 = self#name acc2 name in
      (* skip arity and kind *)
      let r =  { id; location; level; name; arity; kind; } in
      return acc3 r

  method op_def (acc:'a) = function
    | O_module_instance x ->
      let mi, acc1 = self#module_instance acc x in
      let r = O_module_instance mi in
      return acc1 r
    | O_builtin_op x      ->
      let bop, acc1 = self#builtin_op acc x in
      let r = O_builtin_op bop in
      return acc1 r
    | O_user_defined_op x ->
      let uop, acc1 = self#user_defined_op acc x in
      let r = O_user_defined_op uop in
      return acc1 r
    | O_thm_def x ->
      let thdef, acc1 = self#theorem_def acc x in
      let r = O_thm_def thdef in
      return acc1 r
    | O_assume_def x ->
      let adef, acc1 = self#assume_def acc x in
      let r = O_assume_def adef in
      return acc1 r

  method theorem acc0 = function
    | THM_ref x ->
      let tref, acc1 = self#reference acc0 x in
      let r = THM_ref tref in
      return acc1 r

  method theorem_ acc0 {id; location; level; definition; statement; proof;  } =
      let location, acc1 = self#location acc0 location in
      let level, acc2 = opt_map self#level acc1 level in
      let definition, acc3 = opt_map self#theorem_def acc2 definition in
      let statement, acc4 = self#statement acc3 statement in
      let proof, acc5 = self#proof acc4 proof  in
      (* skip suffices *)
      let r = { id; location; level; definition; statement; proof; } in
      return acc5 r

  method statement acc0 = function
    | ST_FORMULA f ->
      let f, acc1 = self#node acc0 f in
      let anys = ST_FORMULA f in
      return acc1 anys
    | ST_SUFFICES f ->
      let f, acc1 = self#node acc0 f in
      let anys = ST_SUFFICES f in
      return acc1 anys
    | ST_CASE f ->
      let f, acc1 = self#expr acc0 f in
      let anys = ST_CASE f in
      return acc1 anys
    | ST_PICK {variables; formula; } ->
      let variables, acc1 = fold self#bound_symbol acc0 variables in
      let formula, acc2 = self#expr acc1 formula in
      let anys = { variables; formula; } in
      return acc2 (ST_PICK anys)
    | ST_HAVE f ->
      let f, acc1 = self#expr acc0 f in
      let anys = ST_HAVE f in
      return acc1 anys
    | ST_TAKE bound_variables ->
      let bound_variables, acc1 =
        fold self#bound_symbol acc0 bound_variables in
      let anys = ST_TAKE bound_variables in
      return acc1 anys
    | ST_WITNESS f ->
      let f, acc1 = self#expr acc0 f in
      let anys = ST_WITNESS f in
      return acc1 anys
    | ST_QED ->
      return acc0 ST_QED

  method assume acc0  = function
    | ASSUME_ref x ->
      let aref, acc1 = self#reference acc0 x in
      let r = ASSUME_ref aref in
      return acc1 r

  method assume_ acc0 {id; location; definition; level; expr; } =
      let location, acc1 = self#location acc0 location in
      let level, acc2 = opt_map self#level acc1 level in
      let definition, acc3 = opt_map self#assume_def acc2 definition in
      let expr, acc = self#expr acc3 expr in
      let r = { id; location; level; definition; expr; } in
      return acc r

  method proof acc0 = function
    | P_omitted { location; level } ->
      let location, acc1 = self#location acc0 location in
      let level, acc2 = opt_map self#level acc1 level in
      let r = P_omitted { location; level; }
      in
      return acc1 r
    | P_obvious { location; level } ->
      let location, acc1 = self#location acc0 location in
      let level, acc2 = opt_map self#level acc1 level in
      let r = P_obvious { location; level; } in
      return acc1 r
    | P_by { location; level; facts; defs; only} ->
      let location, acc1 = self#location acc0 location in
      let level, acc2 = opt_map self#level acc1 level in
      let facts, acc3 =
        fold self#expr_or_module_or_module_instance acc2 facts in
      let defs, acc = fold self#defined_expr acc3 defs in
      (* skip the only tag *)
      let r = P_by { location; level; facts; defs; only; } in
      return acc r
    | P_steps { location; level; steps; } ->
      let location, acc1 = self#location acc0 location in
      let level, acc2 = opt_map self#level acc1 level in
      let steps, acc3 = fold self#step acc2 steps in
      let r = P_steps { location; level; steps; } in
      return acc0 r
    | P_noproof ->
      return acc0 P_noproof

  method step acc0 = function
    | S_def_step x ->
      let step, acc = self#def_step acc0 x in
      let r = S_def_step step in
      return acc r
    | S_use_or_hide x ->
      let step, acc = self#use_or_hide acc0 x in
      let r = S_use_or_hide step in
      return acc r
    | S_instance i ->
      let step, acc = self#instance acc0 i in
      let r = S_instance step in
      return acc r
    | S_theorem t ->
      let step, acc = self#theorem acc0 t in
      let r = S_theorem step in
      return acc r


  method use_or_hide acc0 {  location; level; facts; defs; only; hide } =
    let location, acc1 = self#location acc0 location in
    let level, acc2 = opt_map self#level acc1 level in
    let facts, acc3 = fold self#expr_or_module_or_module_instance acc2 facts in
    let defs, acc = fold self#defined_expr acc3 defs in
    let r = { location; level; facts; defs; only; hide; } in
    return acc r

  method instance acc0 {location; level; name; module_name; substs; params; } =
    let location, acc1 = self#location acc0 location in
    let level, acc2 = opt_map self#level acc1 level in
    let name, acc3 = opt_map self#name acc2 name  in
    let module_name, acc4 = self#name acc3 module_name in
    let substs, acc5 = fold self#instantiation acc4 substs in
    let params, acc = fold self#formal_param acc5 params in
    let r = { location; level; name; module_name; substs; params; } in
    return acc r

  method instantiation acc0 { op; expr; next } =
    let op, acc1 = self#op_decl acc0 op in
    let expr, acc2 = self#expr_or_op_arg acc1 expr in
    let next, acc = fold self#expr_or_op_arg acc2 next in
    let r = { op; expr; next; } in
    return acc r

  method fp_assignment acc0 { param; expr } =
    let param, acc1 = self#formal_param acc0 param in
    let expr, acc = self#expr_or_op_arg acc1 expr in
    let r = { param; expr; } in
    return acc r

  method assume_prove acc0 { location; level; new_symbols; assumes;
                             prove; suffices; boxed; } =
    let location, acc1 = self#location acc0 location in
    let level, acc2 = opt_map self#level acc1 level in
    let new_symbols, acc3 = fold self#new_symb acc2 new_symbols in
    let assumes, acc4 = fold self#node acc3 assumes in
    let prove, acc = self#expr acc4 prove in
    (* suffices and boxed are boolean flags*)
    let r = { location; level; new_symbols; assumes;
              prove; suffices; boxed; } in
    return acc r

  method new_symb acc0 { location; level; op_decl; set } =
    let location, acc1 = self#location acc0 location in
    let level, acc2 = opt_map self#level acc1 level in
    let op_decl, acc3 = self#op_decl acc2 op_decl in
    let set, acc = opt_map self#expr acc3 set in
    let r = { location; level; op_decl; set; } in
    return acc r

  method let_in acc0 {location; level; body; op_defs } =
    let location, acc1 = self#location acc0 location in
    let level, acc2 = opt_map self#level acc1 level in
    let body, acc3 = self#expr acc2 body in
    let op_defs, acc = fold self#op_def_or_theorem_or_assume acc3 op_defs in
    let r = { location; level; body; op_defs; } in
    return acc r

  method subst_in acc0 ({ location; level; substs; body;
                          instantiated_from; instantiated_into } : subst_in) =
    let location, acc1 = self#location acc0 location in
    let level, acc2 = opt_map self#level acc1 level in
    let substs, acc3 = fold self#instantiation acc2 substs in
    let body, acc4 = self#expr acc3 body in
    let instantiated_from, acc5 = self#mule acc4 instantiated_from in
    let instantiated_into, acc6 = self#mule acc5 instantiated_into in
    let r = { location; level; substs; body;
              instantiated_from; instantiated_into; } in
    return acc6 r

  method fp_subst_in acc0 ({ location; level; substs; body } : fp_subst_in) =
    let location, acc1 = self#location acc0 location in
    let level, acc2 = opt_map self#level acc1 level in
    let substs, acc3 = fold self#fp_assignment acc2 substs in
    let body, acc = self#expr acc3 body in
    let r =  { location; level; substs; body; } in
    return acc r

  method label acc0 ({location; level; name; arity; body; params } : label) =
    let location, acc1 = self#location acc0 location in
    let level, acc2 = opt_map self#level acc1 level in
    let name, acc3 = self#name acc2 name in
    (* skip arity *)
    let body, acc4 = self#node acc3 body in
    let params, acc = fold self#formal_param acc4 params in
    let r = { location; level; name; arity; body; params; } in
    return acc r

  method ap_subst_in acc0 ({ location; level; substs; body;
                             instantiated_from; instantiated_into; } : ap_subst_in) =
    let location, acc1 = self#location acc0 location in
    let level, acc2 = opt_map self#level acc1 level in
    let substs, acc3 = fold self#instantiation acc2 substs in
    let body, acc4 = self#node acc3 body in
    let instantiated_from, acc5 = self#mule acc4 instantiated_from in
    let instantiated_into, acc6 = self#mule acc5 instantiated_into in
    let r = ({ location; level; substs; body;
              instantiated_from; instantiated_into; } : ap_subst_in) in
    return acc6 r

  method def_step acc0 { location; level; defs } =
    let location, acc1 = self#location acc0 location in
    let level, acc2 = opt_map self#level acc1 level in
    let defs, acc = fold self#op_def acc2 defs in
    let r = { location; level; defs; } in
    return acc r

  method module_instance acc0 = function
    | MI_ref x ->
      let reference, acc = self#reference acc0 x in
      let r = MI_ref reference in
      return acc r

  method module_instance_ acc0
      ({id; location; level; name} : module_instance_) =
      let location, acc1 = self#location acc0 location in
      let level, acc2 = opt_map self#level acc1 level in
      let name, acc = self#name acc2 name in
      let r = { id; location; level; name; } in
      return acc r

  method builtin_op acc0 = function
    | BOP_ref x ->
      let reference, acc = self#reference acc0 x in
      let r = BOP_ref reference in
      return acc r

  method builtin_op_ acc0 { id; level; name; arity; params } =
      let level, acc1 = opt_map self#level acc0 level in
      let name, acc2 = self#name acc1 name in
      (* skip arity *)
      let fps, leibniz = List.split params in
      let fparams, acc = fold self#formal_param acc2 fps in
      let params = List.combine fparams leibniz in
      let r =  { id; level; name; arity; params; } in
      return acc r

  method user_defined_op acc0 = function
    | UOP_ref x ->
      let reference, acc = self#reference acc0 x in
      let r = UOP_ref reference in
      return acc r

  method user_defined_op_ acc0
    { id; location; level; name; arity;
      body; params; source; recursive; } =
      let location, acc1 = self#location acc0 location in
      let level, acc2 = opt_map self#level acc1 level in
      let name, acc3 = self#name acc2 name in
      (* arity *)
      let body, acc4 = self#expr acc3 body in
      let fps, leibniz = List.split params in
      let fparams, acc5 = fold self#formal_param acc4 fps in
      let params = List.combine fparams leibniz in
      let source, acc6 = opt_map self#user_defined_op acc5 source in
      let r = { id; location; level; name; arity; body; params;
                source; recursive; } in
      return acc6 r

  method assume_def acc0 (ADef_ref x) =
    let reference, acc = self#reference acc0 x in
    let r = ADef_ref reference in
    return acc r

  method assume_def_ acc0 ({id; location; level; name; body} : assume_def_) =
      let location, acc1 = self#location acc0 location in
      let level, acc2 = opt_map self#level acc1 level in
      let name, acc3 = self#name acc2 name in
      let body, acc4 = self#expr acc3 body in
      let r = ({ id; location; level; name; body; } : assume_def_) in
      return acc4 r

  method theorem_def acc0 (TDef_ref x) =
    let reference, acc = self#reference acc0 x in
    let r = TDef_ref reference in
    return acc r

  method theorem_def_ acc0 ({id; location; level; name; body} : theorem_def_) =
      let location, acc1 = self#location acc0 location in
      let level, acc2 = opt_map self#level acc1 level in
      let name, acc3 = self#name acc2 name in
      let body, acc4 = self#node acc3 body in
      let r = { id; location; level; name; body; } in
      return acc4 r

  method entry (acc:'a) (id, e) = match e with
    | FP_entry x ->
      let entry, acc0 = self#formal_param_ acc x in
      return acc0 (id, FP_entry entry)
    | MOD_entry x ->
      let entry, acc0 = self#mule_ acc x in
      return acc0 (id, MOD_entry entry)
    | BOP_entry x ->
      let entry, acc0 = self#builtin_op_ acc x in
      return acc0 (id, BOP_entry entry)
    | UOP_entry x ->
      let entry, acc0 = self#user_defined_op_ acc x in
      return acc0 (id, UOP_entry entry)
    | MI_entry x ->
      let entry, acc0 = self#module_instance_ acc x in
      return acc0 (id, MI_entry entry)
    | TDef_entry x ->
      let entry, acc0 = self#theorem_def_ acc x in
      return acc0 (id, TDef_entry entry)
    | ADef_entry x ->
      let entry, acc0 = self#assume_def_ acc x in
      return acc0 (id, ADef_entry entry)
    | OPDec_entry x ->
      let entry, acc0 = self#op_decl_ acc x in
      return acc0 (id, OPDec_entry entry)
    | THM_entry x ->
      let entry, acc0 = self#theorem_ acc x in
      return acc0 (id, THM_entry entry)
    | ASSUME_entry x ->
      let entry, acc0 = self#assume_ acc x in
      return acc0 (id, ASSUME_entry entry)

  method context (acc:'a) { root_module; entries; modules } =
    let entries, acc1 = fold self#entry acc entries in
    let modules, acc2 = fold self#mule acc1 modules in
    let r = { root_module; entries; modules } in
    return acc2 r

  (* pure disjunction types *)
  method expr acc = function
    | E_at x        ->
      let at, acc = self#at acc x in
      let r = (E_at at) in
      return acc r
    | E_decimal x        ->
      let decimal, acc = self#decimal acc x in
      let r = E_decimal decimal  in
      return acc r
    | E_label x        ->
      let label, acc = self#label acc x in
      let r = E_label label  in
      return acc r
    | E_let_in x        ->
      let let_in, acc = self#let_in acc x in
      let r = E_let_in let_in in
      return acc r
    | E_numeral x        ->
      let numeral, acc = self#numeral acc x in
      let r = E_numeral numeral  in
      return acc r
    | E_op_appl x        ->
      let op_appl, acc = self#op_appl acc x in
      let r = E_op_appl op_appl  in
      return acc r
    | E_string x        ->
      let strng, acc = self#strng acc x in
      let r = E_string strng  in
      return acc r
    | E_subst_in x        ->
      let subst_in, acc = self#subst_in acc x in
      let r = E_subst_in subst_in  in
      return acc r
    | E_fp_subst_in x        ->
      let fp_subst_in, acc = self#fp_subst_in acc x in
      let r = E_fp_subst_in fp_subst_in  in
      return acc r
    | E_binder x        ->
      let binder, acc = self#binder acc x in
      let r = E_binder binder  in
      return acc r

  method op_appl_or_binder acc0 = function
    | OB_op_appl x ->
      let oab, acc0 = self#op_appl acc0 x in
      let r = OB_op_appl oab in
      return acc0 r
    | OB_binder x ->
      let oab, acc0 = self#binder acc0 x in
      let r = OB_binder oab in
      return acc0 r

  method expr_or_module_or_module_instance acc = function
    | EMM_expr x            ->
      let emm, acc0 = self#expr acc x in
      let r = EMM_expr emm in
      return acc0 r
    | EMM_module_instance x ->
      let emm, acc0 = self#module_instance acc x in
      let r = EMM_module_instance emm in
      return acc0 r
    | EMM_module x          ->
      let emm, acc0 = self#mule acc x in
      let r = EMM_module emm in
      return acc0 r

  method defined_expr acc = function
    | UMTA_user_defined_op x ->
      let de, acc0 = self#user_defined_op acc x in
      let r = UMTA_user_defined_op de in
      return acc0 r
    | UMTA_module_instance x ->
      let de, acc0 = self#module_instance acc x in
      let r = UMTA_module_instance de in
      return acc0 r
    | UMTA_theorem_def x         ->
      let de, acc0 = self#theorem_def acc x in
      let r = UMTA_theorem_def de in
      return acc0 r
    | UMTA_assume_def x          ->
      let de, acc0 = self#assume_def acc x in
      let r = UMTA_assume_def de in
      return acc0 r

  method op_def_or_theorem_or_assume acc = function
    | OTA_op_def x ->
      let ota, acc0 = self#op_def acc x in
      let r = OTA_op_def ota in
      return acc0 r
    | OTA_theorem x ->
      let ota, acc0 = self#theorem acc x in
      let r = OTA_theorem ota in
      return acc0 r
    | OTA_assume x ->
      let ota, acc0 = self#assume acc x in
      let r = OTA_assume ota in
      return acc0 r

  method expr_or_op_arg (acc:'a) = function
    | EO_op_arg oa ->
      let eo, acc0 = self#op_arg acc oa in
      let r = EO_op_arg eo in
      return acc0 r
    | EO_expr e ->
      let eo, acc0 = self#expr acc e in
      let r = EO_expr eo in
      return acc0 r

  method operator acc = function
    | FMOTA_formal_param x ->
      let fmota, acc0 = self#formal_param acc x in
      let r = FMOTA_formal_param fmota in
      return acc0 r
    | FMOTA_op_decl x ->
      let fmota, acc0 = self#op_decl acc x in
      let r = FMOTA_op_decl fmota in
      return acc0 r
    | FMOTA_op_def  x ->
      let fmota, acc0 = self#op_def acc x in
      let r = FMOTA_op_def fmota in
      return acc0 r
    | FMOTA_ap_subst_in x ->
      let fmota, acc0 = self#ap_subst_in acc x in
      let r = FMOTA_ap_subst_in fmota in
      return acc0 r
    | FMOTA_lambda x        ->
      let fmota, acc0 = self#lambda acc x in
      let r =FMOTA_lambda fmota in
      return acc0 r

  method mule_entry (acc:'a) = function
    | MODe_op_decl x     ->
      let me, acc = self#op_decl acc x in
      let r = MODe_op_decl me in
      return acc r
    | MODe_op_def x      ->
      let me, acc = self#op_def acc x in
      let r = MODe_op_def me in
      return acc r
    | MODe_assume x      ->
      let me, acc = self#assume acc x in
      let r = MODe_assume me in
      return acc r
    | MODe_theorem x     ->
      let me, acc = self#theorem acc x in
      let r = MODe_theorem me in
      return acc r
    | MODe_instance x    ->
      let me, acc = self#instance acc x in
      let r = MODe_instance me in
      return acc r
    | MODe_use_or_hide x ->
      let me, acc = self#use_or_hide acc x in
      let r = MODe_use_or_hide me in
      return acc r

  method node (acc:'a) = function
    | N_assume_prove x  ->
      let node, acc0 = self#assume_prove acc x in
      let r = N_assume_prove node in
      return acc0 r
    | N_expr x         ->
      let node, acc0 = self#expr acc x in
      let r = N_expr node in
      return acc0 r
    | N_ap_subst_in aps ->
      let node, acc0 = self#ap_subst_in acc aps in
      let r = N_ap_subst_in node in
      return acc0 r


end
