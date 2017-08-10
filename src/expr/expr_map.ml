open Commons
open Expr_ds
open Expr_visitor
open Any_expr
open Util

type 'a macc =  anyExpr * 'a

(** Extracts the anyAxpr from a macc. *)
let get_anyexpr (any,acc) = any

(** Extracts the accumulator for derived classes from a macc. *)
let get_acc (any,acc) = acc

(** Sets the accumulator for derived classes from a macc. *)
let set_acc (any,_) acc = (any, acc)

(** Sets the anyExpr of the first argument to the one given as second. *)
let set_anyexpr (_,acc) any = (any,acc)

(** Sets the anyExpr of the macc passed to that of the second macc. *)
let update_anyexpr (_,acc) (any,_) = (any,acc)

(** This is an any_extractor which extracts the anyExpr from the macc first. *)
class ['b] macc_extractor = object
  inherit ['b macc] any_extractor
  method extract = get_anyexpr
end

(** This is an any_extractor which directly extracts from an anyExpr. *)
class id_extractor = object
  inherit [anyExpr] any_extractor
  method extract x = x
end


let rec mfold ?first:(first=true) (up:anyExpr -> 'c)
    (f:'a macc -> 'b -> 'a macc)
    (start_acc:'a macc)
    (list: 'b list) =
  match list with
  | x::xs ->
    let nacc = f start_acc x in
    let any = get_anyexpr nacc in
    let result = mfold ~first:false up f nacc xs in
    let rany, racc = result in
    (up any :: rany, racc)
  | [] ->
    ([], start_acc)

let fold f acc l =
  let up x = x in
  let anys, racc = mfold up f acc l in
  (anys, racc)

let unpack_fold unpack f acc l =
  let anys, racc = mfold unpack f acc l in
  assert ((List.length l) = (List.length anys));
  (anys, racc)


class ['a] expr_map = object(self)
  inherit ['a macc] visitor as super
  val macc_extract = new macc_extractor
  val id_extract = new id_extractor

  (* values can't be accessed from other modules, need a getter *)
  method get_macc_extractor = macc_extract
  method get_id_extractor = id_extract

  (* parts of expressions *)
  method location acc l  =
    set_anyexpr acc (Any_location l)
  method level acc l  =
    set_anyexpr acc (Any_level l)

  (* non-recursive expressions *)
  method decimal acc d =
    set_anyexpr acc (Any_decimal d)
  method numeral acc n =
    set_anyexpr acc (Any_numeral n)
  method strng acc s =
    set_anyexpr acc (Any_strng s)

  (* recursive expressions *)
  method at acc0 {location; level; except; except_component} =
    let acc1 = self#location acc0 location in
    let acc2 = self#level acc1 level in
    let acc3 = self#op_appl_or_binder acc2 except in
    let acc4 = self#op_appl_or_binder acc3 except_component in
    let r = Any_at {
        location = macc_extract#location acc1;
        level = macc_extract#level acc2;
        except = macc_extract#op_appl_or_binder acc3;
        except_component = macc_extract#op_appl_or_binder acc4;
      } in
    set_anyexpr acc4 r

  method op_appl acc0 ({location; level; operator; operands} : op_appl) =
    let acc1 = self#location acc0 location in
    let acc2 = self#level acc1 level in
    let acc3 = self#operator acc2 operator in
    let ops, acc = unpack_fold id_extract#expr_or_op_arg
        self#expr_or_op_arg acc3 operands in
    let r = Any_op_appl {
        location = macc_extract#location acc1;
        level = macc_extract#level acc2;
        operator = macc_extract#operator acc3;
        operands = ops;
      } in
    set_anyexpr acc r


  method binder acc0 {location; level; operator; operand; bound_symbols} =
    let acc1 = self#location acc0 location in
    let acc2 = self#level acc1 level in
    let acc3 = self#operator acc2 operator in
    let acc4 = self#expr_or_op_arg acc3 operand in
    let bound_symbols, acc = unpack_fold id_extract#bound_symbol
        self#bound_symbol
        acc4 bound_symbols in
    let r = Any_binder {
        location = macc_extract#location acc1;
        level = macc_extract#level acc2;
        operator = macc_extract#operator acc3;
        operand = macc_extract#expr_or_op_arg acc4;
        bound_symbols = bound_symbols;
      } in
    set_anyexpr acc r

  method lambda acc ({location; level; arity; body; params; } : lambda) =
    let acc0 = self#location acc location in
    let acc1 = self#level acc0 level in
    (* arity *)
    let acc2 = self#expr acc1 body in
    let fparams, acc = unpack_fold id_extract#formal_param
        (fun x (fp,_) -> self#formal_param x fp)
        acc2 params in
    let leibnizflags = List.map snd params in
    let r = Any_lambda {
        location = macc_extract#location acc0;
        level = macc_extract#level acc1;
        arity = arity;
        body = macc_extract#expr acc2;
        params = List.combine fparams leibnizflags;
      } in
    set_anyexpr acc r

  method bound_symbol acc = function
    | B_bounded_bound_symbol s ->
      let any, acc1 = self#bounded_bound_symbol acc s in
      let bs = B_bounded_bound_symbol (id_extract#bounded_bound_symbol any) in
      (Any_bound_symbol bs, acc1)
    | B_unbounded_bound_symbol s ->
      let any, acc1 = self#unbounded_bound_symbol acc s in
      let ubs = B_unbounded_bound_symbol
          (id_extract#unbounded_bound_symbol any) in
      (Any_bound_symbol ubs, acc1)

  method bounded_bound_symbol acc { params; tuple; domain } =
    let params, acc0 =
      unpack_fold id_extract#formal_param self#formal_param acc params in
    let acc1 = self#expr acc domain in
    let r = Any_bounded_bound_symbol {
        params;
        tuple;
        domain = macc_extract#expr acc1;
      } in
    set_anyexpr acc1 r

  method unbounded_bound_symbol acc { param; tuple}  =
    let acc0 = self#formal_param acc param in
    let r = Any_unbounded_bound_symbol {
        param = macc_extract#formal_param acc0;
        tuple;
      } in
    set_anyexpr acc0 r

  method formal_param acc0 = function
    | FP_ref i ->
      let acc1 = self#reference acc0 i in
      let r = Any_formal_param (FP_ref (macc_extract#reference acc1)) in
      set_anyexpr acc1 r

  method formal_param_ acc0 { id; location; level; name; arity; } =
      let acc1 = self#location acc0 location in
      let acc2 = self#level acc1 level in
      let acc3 = self#name acc2 name in
      (* arity skipped *)
      let r = Any_formal_param_
          ({
              id;
              location = macc_extract#location acc1;
              level = macc_extract#level acc2;
              name = macc_extract#name acc3;
              arity;
            }) in
      set_anyexpr acc3 r

  method mule acc0 = function
    | MOD_ref i ->
      let acc1 = self#reference acc0 i in
      let r = Any_mule (MOD_ref (macc_extract#reference acc1)) in
      set_anyexpr acc1 r

  method mule_ acc0 {id; name; location; module_entries } =
      let acc1 = self#name acc0 name in
      let acc2 = self#location acc1 location in
      let m_entries,   acc =
        unpack_fold id_extract#mule_entry self#mule_entry acc2 module_entries in
      let r = Any_mule_ ({
          id;
          name = macc_extract#name acc1;
          location = macc_extract#location acc2;
          module_entries = m_entries;
        }) in
      set_anyexpr acc r

  method op_arg acc0 {location; level; argument } =
    (* terminal node *)
    let acc1 = self#location acc0 location in
    let acc2 = self#level acc1 level in
    let acc3 = self#operator acc2 argument in
    let r = Any_op_arg {
        location = macc_extract#location acc1;
        level = macc_extract#level acc2;
        argument = macc_extract#operator acc3;
      } in
    set_anyexpr acc3 r

  method op_decl acc0 = function
    | OPD_ref x ->
      let acc1 = self#reference acc0 x in
      let r = Any_op_decl (OPD_ref (macc_extract#reference acc1)) in
      set_anyexpr acc1 r

  method op_decl_ acc0 { id; location ; level ; name ; arity ; kind ; } =
      (* terminal node *)
      let acc1 = self#location acc0 location in
      let acc2 = self#level acc1 level in
      let acc3 = self#name acc2 name in
      (* skip arity and kind *)
      let r = Any_op_decl_
          ({
            id;
            location = macc_extract#location acc1;
            level = macc_extract#level acc2;
            name = macc_extract#name acc3;
            arity;
            kind;
          }) in
      set_anyexpr acc3 r

  method op_def acc = function
    | O_module_instance x ->
      let acc1 = self#module_instance acc x in
      let r = Any_op_def
          (O_module_instance (macc_extract#module_instance acc1)) in
      set_anyexpr acc1 r
    | O_builtin_op x      ->
      let acc1 = self#builtin_op acc x in
      let r = Any_op_def
          (O_builtin_op (macc_extract#builtin_op acc1)) in
      set_anyexpr acc1 r
    | O_user_defined_op x ->
      let acc1 = self#user_defined_op acc x in
      let r = Any_op_def
          (O_user_defined_op (macc_extract#user_defined_op acc1)) in
      set_anyexpr acc1 r
    | O_thm_def x ->
      let acc1 = self#theorem_def acc x in
      let r = Any_op_def
          (O_thm_def (macc_extract#theorem_def acc1)) in
      set_anyexpr acc1 r
    | O_assume_def x ->
      let acc1 = self#assume_def acc x in
      let r = Any_op_def
          (O_assume_def (macc_extract#assume_def acc1)) in
      set_anyexpr acc1 r

  method theorem acc0 = function
    | THM_ref x ->
      let acc1 = self#reference acc0 x in
      let r = Any_theorem (THM_ref (macc_extract#reference acc1)) in
      set_anyexpr acc1 r

  method theorem_ acc0 {id; location; level; definition; statement; proof;  } =
      let acc1 = self#location acc0 location in
      let acc2 = self#level acc1 level in
      let definition, acc3 = match definition with
        | None -> None, acc2
        | Some n ->
          let acc3_ = self#theorem_def acc2 n in
          (Some (macc_extract#theorem_def acc3_), acc3_)
      in
      let acc4 = self#statement acc3 statement in
      let acc5 = self#proof acc4 proof  in
      (* skip suffices *)
      let r = Any_theorem_ ({
          id;
          location = macc_extract#location acc1;
          level = macc_extract#level acc2;
          definition;
          statement = macc_extract#statement acc4;
          proof = macc_extract#proof acc5;
        }) in
      set_anyexpr acc5 r

  method statement acc0 = function
    | ST_FORMULA f ->
      let acc1 = self#node acc0 f in
      let anys = (Any_statement (ST_FORMULA (macc_extract#node acc1)))
      in set_anyexpr acc1 anys
    | ST_SUFFICES f ->
      let acc1 = self#node acc0 f in
      let anys = (Any_statement (ST_SUFFICES (macc_extract#node acc1)))
      in set_anyexpr acc1 anys
    | ST_CASE f ->
      let acc1 = self#expr acc0 f in
      let anys = (Any_statement (ST_CASE (macc_extract#expr acc1))) in
      set_anyexpr acc1 anys
    | ST_PICK {variables; formula; } ->
      let (variables, acc1) = unpack_fold id_extract#bound_symbol
          self#bound_symbol acc0 variables in
      let acc2 = self#expr acc1 formula in
      let anys = {
        variables;
        formula = macc_extract#expr acc2;
      } in
      set_anyexpr acc2 (Any_statement (ST_PICK anys))
    | ST_HAVE f ->
      let acc1 = self#expr acc0 f in
      let anys = (Any_statement (ST_HAVE (macc_extract#expr acc1))) in
      set_anyexpr acc1 anys
    | ST_TAKE bound_variables ->
      let (bound_variables, acc1) = unpack_fold id_extract#bound_symbol
          self#bound_symbol acc0 bound_variables in
      let anys = (Any_statement (ST_TAKE bound_variables)) in
      set_anyexpr acc1 anys
    | ST_WITNESS f ->
      let acc1 = self#expr acc0 f in
      let anys = (Any_statement (ST_WITNESS (macc_extract#expr acc1))) in
      set_anyexpr acc1 anys
    | ST_QED ->
      let anys = (Any_statement ST_QED) in
      set_anyexpr acc0 anys

  method assume acc0  = function
    | ASSUME_ref x ->
      let acc1 = self#reference acc0 x in
      let r = Any_assume (ASSUME_ref (macc_extract#reference acc1)) in
      set_anyexpr acc1 r

  method assume_ acc0 {id; location; definition; level; expr; } =
      let acc1 = self#location acc0 location in
      let acc2 = self#level acc1 level in
      let acc3 = CCOpt.map_or ~default:acc2 (self#assume_def acc2) definition in
      let acc = self#expr acc3 expr in
      let r = Any_assume_ ({
          id;
          location = macc_extract#location acc1;
          level = macc_extract#level acc2;
          definition = CCOpt.map (fun _ -> macc_extract#assume_def acc3) definition;
          expr = macc_extract#expr acc;
        }) in
      set_anyexpr acc r

  method proof acc0 = function
    | P_omitted { location; level } ->
      let acc1 = self#location acc0 location in
      let acc2 = self#level acc1 level in
      let r = Any_proof (
          P_omitted {
            location = macc_extract#location acc1;
            level = macc_extract#level acc2;
          })
      in
      set_anyexpr acc1 r
    | P_obvious { location; level } ->
      let acc1 = self#location acc0 location in
      let acc2 = self#level acc1 level in
      let r = Any_proof (
          P_obvious {
            location = macc_extract#location acc1;
            level = macc_extract#level acc2;
          })
      in
      set_anyexpr acc1 r
    | P_by { location; level; facts; defs; only} ->
      let acc1 = self#location acc0 location in
      let acc2 = self#level acc1 level in
      let facts, acc3 =
        unpack_fold id_extract#expr_or_module_or_module_instance
          self#expr_or_module_or_module_instance acc2 facts in
      let defs, acc = unpack_fold id_extract#defined_expr
          self#defined_expr acc3 defs in
      (* skip the only tag *)
      let r = Any_proof (P_by {
          location = macc_extract#location acc1;
          level = macc_extract#level acc2;
          facts;
          defs;
          only;
        }) in
      set_anyexpr acc r
    | P_steps { location; level; steps; } ->
      let acc1 = self#location acc0 location in
      let acc2 = self#level acc1 level in
      let steps, acc3 = unpack_fold id_extract#step self#step acc2 steps in
      let r = Any_proof (
          P_steps {
            location = macc_extract#location acc1;
            level = macc_extract#level acc2;
            steps;
          }) in
      set_anyexpr acc0 r
    | P_noproof ->
      set_anyexpr acc0 (Any_proof P_noproof)

  method step acc0 = function
    | S_def_step x ->
      let acc = self#def_step acc0 x in
      let r = Any_step (S_def_step (macc_extract#def_step acc)) in
      set_anyexpr acc r
    | S_use_or_hide x ->
      let acc = self#use_or_hide acc0 x in
      let r = Any_step (S_use_or_hide (macc_extract#use_or_hide acc)) in
      set_anyexpr acc r
    | S_instance i ->
      let acc = self#instance acc0 i in
      let r = Any_step (S_instance (macc_extract#instance acc)) in
      set_anyexpr acc r
    | S_theorem t ->
      let acc = self#theorem acc0 t in
      let r = Any_step (S_theorem (macc_extract#theorem acc)) in
      set_anyexpr acc r


  method use_or_hide acc0 {  location; level; facts; defs; only; hide } =
    let acc1 = self#location acc0 location in
    let acc2 = self#level acc1 level in
    let facts, acc3 = unpack_fold id_extract#expr_or_module_or_module_instance
        self#expr_or_module_or_module_instance acc2 facts in
    let defs, acc = unpack_fold id_extract#defined_expr
        self#defined_expr acc3 defs in
    let r = Any_use_or_hide {
        location = macc_extract#location acc1;
        level = macc_extract#level acc2;
        facts;
        defs;
        only;
        hide;
      } in
    set_anyexpr acc r

  method instance acc0 {location; level; name; module_name; substs; params; } =
    let acc1 = self#location acc0 location in
    let acc2 = self#level acc1 level in
    let acc3 = match name with
      | None -> acc2
      | Some name -> self#name acc2 name
    in
    let acc4 = self#name acc3 module_name in
    let name =  match name with
      | None -> None
      | Some _ -> Some (macc_extract#name acc3)
    in
    let substs, acc5 = unpack_fold id_extract#instantiation
        self#instantiation acc4 substs in
    let params, acc = unpack_fold id_extract#formal_param
        self#formal_param acc5 params in
    let r = Any_instance {
        location = macc_extract#location acc1;
        level = macc_extract#level acc2;
        name;
        module_name = macc_extract#name acc4;
        substs;
        params;
      } in
    set_anyexpr acc r

  method instantiation acc0 { op; expr } =
    let acc1 = self#op_decl acc0 op in
    let acc = self#expr_or_op_arg acc1 expr in
    let r = Any_instantiation {
        op = macc_extract#op_decl acc1 ;
        expr = macc_extract#expr_or_op_arg acc ;
      } in
    set_anyexpr acc r

  method fp_assignment acc0 { param; expr } =
    let acc1 = self#formal_param acc0 param in
    let acc = self#expr_or_op_arg acc1 expr in
    let r = Any_fp_assignment {
        param = macc_extract#formal_param acc1 ;
        expr = macc_extract#expr_or_op_arg acc ;
      } in
    set_anyexpr acc r

  method assume_prove acc0 { location; level; new_symbols; assumes;
                             prove; suffices; boxed; } =
    let acc1 = self#location acc0 location in
    let acc2 = self#level acc1 level in
    let new_symbols, acc3 = unpack_fold id_extract#new_symb
        self#new_symb acc2 new_symbols in
    let assumes, acc4 = unpack_fold id_extract#node
        self#node acc3 assumes in
    let acc = self#expr acc4 prove in
    (* suffices and boxed are boolean flags*)
    let r = Any_assume_prove {
        location = macc_extract#location acc1;
        level = macc_extract#level acc2;
        new_symbols;
        assumes;
        prove = macc_extract#expr acc;
        suffices;
        boxed;
      } in
    set_anyexpr acc r

  method new_symb acc0 { location; level; op_decl; set } =
    let acc1 = self#location acc0 location in
    let acc2 = self#level acc1 level in
    let acc3 = self#op_decl acc2 op_decl in
    let acc = match set with
      | None -> acc3
      | Some e -> self#expr acc3 e
    in
    let r = Any_new_symb {
        location = macc_extract#location acc1;
        level = macc_extract#level acc2;
        op_decl = macc_extract#op_decl acc3;
        set = match set with
          | None -> None
          | _ -> Some (macc_extract#expr acc);
      } in
    set_anyexpr acc r

  method let_in acc0 {location; level; body; op_defs } =
    let acc1 = self#location acc0 location in
    let acc2 = self#level acc1 level in
    let acc3 = self#expr acc2 body in
    let op_defs, acc = unpack_fold id_extract#op_def_or_theorem_or_assume
        self#op_def_or_theorem_or_assume
        acc3 op_defs in
    let r = Any_let_in {
        location = macc_extract#location acc1;
        level = macc_extract#level acc2;
        body = macc_extract#expr acc3;
        op_defs;
      } in
    set_anyexpr acc r

  method subst_in acc0 ({ location; level; substs; body } : subst_in) =
    let acc1 = self#location acc0 location in
    let acc2 = self#level acc1 level in
    let substs, acc3 = unpack_fold id_extract#instantiation
        self#instantiation acc2 substs in
    let acc = self#expr acc3 body in
    let r = Any_subst_in {
        location = macc_extract#location acc1;
        level = macc_extract#level acc2;
        substs;
        body = macc_extract#expr acc;
      } in
    set_anyexpr acc r

  method fp_subst_in acc0 ({ location; level; substs; body } : fp_subst_in) =
    let acc1 = self#location acc0 location in
    let acc2 = self#level acc1 level in
    let substs, acc3 = unpack_fold id_extract#fp_assignment
        self#fp_assignment acc2 substs in
    let acc = self#expr acc3 body in
    let r = Any_fp_subst_in {
        location = macc_extract#location acc1;
        level = macc_extract#level acc2;
        substs;
        body = macc_extract#expr acc;
      } in
    set_anyexpr acc r

  method label acc0 ({location; level; name; arity; body; params } : label) =
    let acc1 = self#location acc0 location in
    let acc2 = self#level acc1 level in
    let acc3 = self#name acc2 name in
    (* skip arity *)
    let acc4 = self#node acc3 body in
    let params, acc = unpack_fold id_extract#formal_param
        self#formal_param acc4 params in
    let r = Any_label {
        location = macc_extract#location acc1;
        level = macc_extract#level acc2;
        name = macc_extract#name acc3;
        arity;
        body = macc_extract#node acc4;
        params;
      } in
    set_anyexpr acc r

  method ap_subst_in acc0 ({ location; level; substs; body } : ap_subst_in) =
    let acc1 = self#location acc0 location in
    let acc2 = self#level acc1 level in
    let substs, acc3 = unpack_fold id_extract#instantiation
        self#instantiation acc2 substs in
    let acc = self#node acc3 body in
    let r = Any_ap_subst_in {
        location = macc_extract#location acc1;
        level = macc_extract#level acc2;
        substs;
        body = macc_extract#node acc;
      } in
    set_anyexpr acc r

  method def_step acc0 { location; level; defs } =
    let acc1 = self#location acc0 location in
    let acc2 = self#level acc1 level in
    let defs, acc = unpack_fold id_extract#op_def
        self#op_def acc2 defs in
    let r = Any_def_step {
        location = macc_extract#location acc1;
        level = macc_extract#level acc2;
        defs;
      } in
    set_anyexpr acc r

  method module_instance acc0 = function
    | MI_ref x ->
      let acc = self#reference acc0 x in
      let r = Any_module_instance (MI_ref (macc_extract#reference acc)) in
      set_anyexpr acc r

  method module_instance_ acc0 {id; location; level; name} =
      let acc1 = self#location acc0 location in
      let acc2 = self#level acc1 level in
      let acc = self#name acc2 name in
      let r = Any_module_instance_ ({
          id;
          location = macc_extract#location acc1;
          level = macc_extract#level acc2;
          name = macc_extract#name acc;
        }) in
      set_anyexpr acc r

  method builtin_op acc0 = function
    | BOP_ref x ->
      let acc = self#reference acc0 x in
      let r = Any_builtin_op (BOP_ref (macc_extract#reference acc)) in
      set_anyexpr acc r
      
  method builtin_op_ acc0 { id; level; name; arity; params } =
      let acc1 = self#level acc0 level in
      let acc2 = self#name acc1 name in
      (* skip arity *)
      let fps, leibniz = List.split params in
      let fparams, acc = unpack_fold id_extract#formal_param
          self#formal_param acc2 fps in
      let params = List.combine fparams leibniz in
      let r = Any_builtin_op_ {
          id;
          level = macc_extract#level acc1;
          name = macc_extract#name acc2;
          arity;
          params;
        } in
      set_anyexpr acc r

  method user_defined_op acc0 = function
    | UOP_ref x ->
      let acc = self#reference acc0 x in
      let r = (Any_user_defined_op (UOP_ref (macc_extract#reference acc))) in
      set_anyexpr acc r

  method user_defined_op_ acc0
    { id; location; level ; name ; arity ;
      body ; params ;  recursive; } =
      let acc1 = self#location acc0 location in
      let acc2 = self#level acc1 level in
      let acc3 = self#name acc2 name in
      (* arity *)
      let acc4 = self#expr acc3 body in
      let fps, leibniz = List.split params in
      let fparams, acc = unpack_fold id_extract#formal_param
          self#formal_param acc4 fps in
      let params = List.combine fparams leibniz in
      let r = Any_user_defined_op_ ({
          id;
          location = macc_extract#location acc1;
          level = macc_extract#level acc2;
          name = macc_extract#name acc3;
          arity;
          body = macc_extract#expr acc4;
          params;
          recursive;
        }) in
      set_anyexpr acc r

  method assume_def acc0 (ADef_ref x) =
    let acc = self#reference acc0 x in
    let r = (Any_assume_def (ADef_ref (macc_extract#reference acc))) in
    set_anyexpr acc r

  method assume_def_ acc0 {id; location; level; name; body} =
      let acc1 = self#location acc0 location in
      let acc2 = self#level acc1 level in
      let acc3 = self#name acc2 name in
      let acc4 = self#expr acc3 body in
      let r = Any_assume_def_ {
          id;
          location = macc_extract#location acc1;
          level = macc_extract#level acc2;
          name = macc_extract#name acc3;
          body = macc_extract#expr acc4;
        }
      in
      set_anyexpr acc4 r
   

  method theorem_def acc0 (TDef_ref x) =
    let acc = self#reference acc0 x in
    let r = (Any_theorem_def (TDef_ref (macc_extract#reference acc))) in
    set_anyexpr acc r

  method theorem_def_ acc0 {id; location; level; name; body} =
      let acc1 = self#location acc0 location in
      let acc2 = self#level acc1 level in
      let acc3 = self#name acc2 name in
      let acc4 = self#node acc3 body in
      let r = Any_theorem_def_ {
          id;
          location = macc_extract#location acc1;
          level = macc_extract#level acc2;
          name = macc_extract#name acc3;
          body = macc_extract#node acc4;
        }
      in
      set_anyexpr acc4 r

  method name acc x =
    set_anyexpr acc (Any_name x)

  method reference acc x =
    set_anyexpr acc (Any_reference x)

  method entry acc (id, e) = match e with
    | FP_entry x ->
      let acc0 = self#formal_param_ acc x in
      let entry = id_extract#formal_param_ (get_anyexpr acc0)
      in
      set_anyexpr acc0 (Any_entry (id, FP_entry entry))
    | MOD_entry x ->
      let acc0 = self#mule_ acc x in
      let entry = id_extract#mule_ (get_anyexpr acc0)
      in
      set_anyexpr acc0 (Any_entry (id, MOD_entry entry))
    | BOP_entry x ->
      let acc0 = self#builtin_op_ acc x in
      let entry = id_extract#builtin_op_ (get_anyexpr acc0) in
      set_anyexpr acc0 (Any_entry (id, BOP_entry entry)) 
    | UOP_entry x ->
      let acc0 = self#user_defined_op_ acc x in
      let entry = id_extract#user_defined_op_ (get_anyexpr acc0) in
      set_anyexpr acc0 (Any_entry (id, UOP_entry entry)) 
    | MI_entry x ->
      let acc0 = self#module_instance_ acc x in
      let entry = id_extract#module_instance_ (get_anyexpr acc0) in
      set_anyexpr acc0 (Any_entry (id, MI_entry entry)) 
    | TDef_entry x ->
      let acc0 = self#theorem_def_ acc x in
      let entry = id_extract#theorem_def_ (get_anyexpr acc0) in
      set_anyexpr acc0 (Any_entry (id, TDef_entry entry)) 
    | ADef_entry x ->
      let acc0 = self#assume_def_ acc x in
      let entry = id_extract#assume_def_ (get_anyexpr acc0) in
      set_anyexpr acc0 (Any_entry (id, ADef_entry entry)) 
    | OPDec_entry x ->
      let acc0 = self#op_decl_ acc x in
      let entry = id_extract#op_decl_ (get_anyexpr acc0)
      in
      set_anyexpr acc0 (Any_entry (id, OPDec_entry entry))
    | THM_entry x ->
      let acc0 = self#theorem_ acc x in
      let entry = id_extract#theorem_ (get_anyexpr acc0)
      in
      set_anyexpr acc0 (Any_entry (id, THM_entry entry))
    | ASSUME_entry x ->
      let acc0 = self#assume_ acc x in
      let entry = id_extract#assume_ (get_anyexpr acc0)
      in
      set_anyexpr acc0 (Any_entry (id, ASSUME_entry entry))

  method context acc { root_module; entries; modules } =
    let entries, acc1 = unpack_fold id_extract#entry self#entry acc entries in
    let modules, acc2 = unpack_fold id_extract#mule self#mule acc1 modules in
    let r = Any_context { root_module; entries; modules } in
    set_anyexpr acc2 r

  (* pure disjunction types *)
  method expr acc = function
    | E_at x        ->
      let acc = self#at acc x in
      let r = Any_expr (E_at (macc_extract#at acc)) in
      set_anyexpr acc r
    | E_decimal x        ->
      let acc = self#decimal acc x in
      let r = Any_expr (E_decimal (macc_extract#decimal acc)) in
      set_anyexpr acc r
    | E_label x        ->
      let acc = self#label acc x in
      let r = Any_expr (E_label (macc_extract#label acc)) in
      set_anyexpr acc r
    | E_let_in x        ->
      let acc = self#let_in acc x in
      let r = Any_expr (E_let_in (macc_extract#let_in acc)) in
      set_anyexpr acc r
    | E_numeral x        ->
      let acc = self#numeral acc x in
      let r = Any_expr (E_numeral (macc_extract#numeral acc)) in
      set_anyexpr acc r
    | E_op_appl x        ->
      let acc = self#op_appl acc x in
      let r = Any_expr (E_op_appl (macc_extract#op_appl acc)) in
      set_anyexpr acc r
    | E_string x        ->
      let acc = self#strng acc x in
      let r = Any_expr (E_string (macc_extract#strng acc)) in
      set_anyexpr acc r
    | E_subst_in x        ->
      let acc = self#subst_in acc x in
      let r = Any_expr (E_subst_in (macc_extract#subst_in acc)) in
      set_anyexpr acc r
    | E_fp_subst_in x        ->
      let acc = self#fp_subst_in acc x in
      let r = Any_expr (E_fp_subst_in (macc_extract#fp_subst_in acc)) in
      set_anyexpr acc r
    | E_binder x        ->
      let acc = self#binder acc x in
      let r = Any_expr (E_binder (macc_extract#binder acc)) in
      set_anyexpr acc r

  method op_appl_or_binder acc0 = function
    | OB_op_appl x ->
      let acc0 = self#op_appl acc0 x in
      let r = OB_op_appl (macc_extract#op_appl acc0) in
      set_anyexpr acc0 (Any_op_appl_or_binder r)
    | OB_binder x ->
      let acc0 = self#binder acc0 x in
      let r = OB_binder (macc_extract#binder acc0) in
      set_anyexpr acc0 (Any_op_appl_or_binder r)

  method expr_or_module_or_module_instance acc = function
    | EMM_expr x            ->
      let acc0 = self#expr acc x in
      let r = EMM_expr (macc_extract#expr acc0) in
      set_anyexpr acc0 (Any_expr_or_module_or_module_instance r)
    | EMM_module_instance x ->
      let acc0 = self#module_instance acc x in
      let r = EMM_module_instance (macc_extract#module_instance acc0) in
      set_anyexpr acc0 (Any_expr_or_module_or_module_instance r)
    | EMM_module x          ->
      let acc0 = self#mule acc x in
      let r = EMM_module (macc_extract#mule acc0) in
      set_anyexpr acc0 (Any_expr_or_module_or_module_instance r)

  method defined_expr acc = function
    | UMTA_user_defined_op x ->
      let acc0 = self#user_defined_op acc x in
      let r = UMTA_user_defined_op (macc_extract#user_defined_op acc0) in
      set_anyexpr acc0 (Any_defined_expr r)
    | UMTA_module_instance x ->
      let acc0 = self#module_instance acc x in
      let r = UMTA_module_instance (macc_extract#module_instance acc0) in
      set_anyexpr acc0 (Any_defined_expr r)
    | UMTA_theorem_def x         ->
      let acc0 = self#theorem_def acc x in
      let r = UMTA_theorem_def (macc_extract#theorem_def acc0) in
      set_anyexpr acc0 (Any_defined_expr r)
    | UMTA_assume_def x          ->
      let acc0 = self#assume_def acc x in
      let r = UMTA_assume_def (macc_extract#assume_def acc0) in
      set_anyexpr acc0 (Any_defined_expr r)

  method op_def_or_theorem_or_assume acc = function
    | OTA_op_def x ->
      let acc0 = self#op_def acc x in
      let r = OTA_op_def (macc_extract#op_def acc0) in
      set_anyexpr acc0 (Any_op_def_or_theorem_or_assume r)
    | OTA_theorem x ->
      let acc0 = self#theorem acc x in
      let r = OTA_theorem (macc_extract#theorem acc0) in
      set_anyexpr acc0 (Any_op_def_or_theorem_or_assume r)
    | OTA_assume x ->
      let acc0 = self#assume acc x in
      let r = OTA_assume (macc_extract#assume acc0) in
      set_anyexpr acc0 (Any_op_def_or_theorem_or_assume r)

  method expr_or_op_arg acc = function
    | EO_op_arg oa ->
      let acc0 = self#op_arg acc oa in
      let r = EO_op_arg (macc_extract#op_arg acc0) in
      set_anyexpr acc0 (Any_expr_or_op_arg r)
    | EO_expr e ->
      let acc0 = self#expr acc e in
      let r = EO_expr (macc_extract#expr acc0) in
      set_anyexpr acc0 (Any_expr_or_op_arg r)

  method operator acc = function
    | FMOTA_formal_param x ->
      let acc0 = self#formal_param acc x in
      let r = FMOTA_formal_param (macc_extract#formal_param acc0) in
      set_anyexpr acc0 (Any_operator r)
(*
    | FMOTA_module  x ->
      let acc0 = self#mule acc x in
      let r = FMOTA_module (macc_extract#mule acc0) in
      set_anyexpr acc0 (Any_operator r)
*)
    | FMOTA_op_decl x ->
      let acc0 = self#op_decl acc x in
      let r = FMOTA_op_decl (macc_extract#op_decl acc0) in
      set_anyexpr acc0 (Any_operator r)
    | FMOTA_op_def  x ->
      let acc0 = self#op_def acc x in
      let r = FMOTA_op_def (macc_extract#op_def acc0) in
      set_anyexpr acc0 (Any_operator r)
(*        
    | FMOTA_theorem x ->
      let acc0 = self#theorem acc x in
      let r = FMOTA_theorem (macc_extract#theorem acc0) in
      set_anyexpr acc0 (Any_operator r)
    | FMOTA_assume  x ->
      let acc0 = self#assume acc x in
      let r = FMOTA_assume (macc_extract#assume acc0) in
      set_anyexpr acc0 (Any_operator r)
*)
    | FMOTA_ap_subst_in x ->
      let acc0 = self#ap_subst_in acc x in
      let r = FMOTA_ap_subst_in (macc_extract#ap_subst_in acc0) in
      set_anyexpr acc0 (Any_operator r)
    | FMOTA_lambda x        ->
      let acc = self#lambda acc x in
      let r =FMOTA_lambda (macc_extract#lambda acc) in
      set_anyexpr acc (Any_operator r)

  method mule_entry acc = function
    | MODe_op_decl x     ->
      let acc = self#op_decl acc x in
      let r = MODe_op_decl (macc_extract#op_decl acc) in
      set_anyexpr acc (Any_mule_entry r)
    | MODe_op_def x      ->
      let acc = self#op_def acc x in
      let r = MODe_op_def (macc_extract#op_def acc) in
      set_anyexpr acc (Any_mule_entry r)
    | MODe_assume x      ->
      let acc = self#assume acc x in
      let r = MODe_assume (macc_extract#assume acc) in
      set_anyexpr acc (Any_mule_entry r)
    | MODe_theorem x     ->
      let acc = self#theorem acc x in
      let r = MODe_theorem (macc_extract#theorem acc) in
      set_anyexpr acc (Any_mule_entry r)
    | MODe_instance x    ->
      let acc = self#instance acc x in
      let r = MODe_instance (macc_extract#instance acc) in
      set_anyexpr acc (Any_mule_entry r)
    | MODe_use_or_hide x ->
      let acc = self#use_or_hide acc x in
      let r = MODe_use_or_hide (macc_extract#use_or_hide acc) in
      set_anyexpr acc (Any_mule_entry r)

  method node acc = function
    | N_assume_prove x  ->
      let acc0 = self#assume_prove acc x in
      let r = N_assume_prove (macc_extract#assume_prove acc0) in
      set_anyexpr acc0 (Any_node r)
    | N_expr x         ->
      let acc0 = self#expr acc x in
      let r = N_expr (macc_extract#expr acc0) in
      set_anyexpr acc0 (Any_node r)
    | N_ap_subst_in aps ->
      let acc0 = self#ap_subst_in acc aps in
      let r = N_ap_subst_in (macc_extract#ap_subst_in acc0) in
      set_anyexpr acc0 (Any_node r)
end
