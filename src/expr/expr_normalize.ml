open Util
open Expr_ds
open Expr_constructors
open Expr_dereference
(* open Expr_substitution *)

module EMap = Expr_map2

let where = "expr_normalize"

type sub_stack =
  | FP_subst of Expr_ds.fp_assignment list
  | OP_subst of Expr_ds.mule * Expr_ds.mule * Expr_ds.instantiation list

(* The accumulator holds:
   * the term db (for dereferencing, may be updated)
   * the current stack of substitutions / instantiations to apply
   * the recursion limit (static)
   * a mapping from user defined operator ids to the number of their unfolding.
     ids not contained in the map will not be unfolded.
   * a list of forbidden names during name generation (may be updated)
   * a set of forbidden ids to generate, to speed up generation of new operators
     (may be updated)
   * a container to be filled by derived classes
*)
type 'a nacc = {
  term_db : term_db;
  current_chain : sub_stack list;
  recursion_limit : int;
  unfolded_uops : int IntMap.t;
  forbidden_names : StringSet.t;
  forbidden_ids   : IntSet.t;
  inner_container : 'a;
}

let get_termdb acc = acc.term_db
let get_sub_stack acc = acc.current_chain
let get_recursion_limit acc = acc.recursion_limit
let get_unfolded_defs acc = acc.unfolded_uops

let update_term_db term_db acc = { acc with term_db }
let update_sub_stack current_chain acc = { acc with current_chain }
let update_unfolded_defs unfolded_uops acc = { acc with unfolded_uops }

let inc_id_counter id nacc =
  let map = get_unfolded_defs nacc in
  let idcount = IntMap.get_or
      ~default:(failwith "Tried to increase untracked id count!") id map in
  let new_map = IntMap.add id (idcount+1) map in
  update_unfolded_defs new_map nacc

let within_recursion_limit uop nacc =
  let term_db = get_termdb nacc in
  let uopi = Deref.user_defined_op term_db uop in
  (not uopi.recursive) ||
  begin
    let limit = get_recursion_limit nacc in
    let unfolded_defs = get_unfolded_defs nacc in
    let count = IntMap.get_or ~default:limit uopi.id unfolded_defs in
    count < limit
  end

let reset_sub_stack acc chain =
  update_sub_stack chain acc



let check_fpsubst fp_substs =
  (* checks if all formal parameter references are unique *)
  let ids = List.fold_left
      (fun set -> function | {param = FP_ref id; expr;} ->
          Util.IntSet.add id set)
      Util.IntSet.empty fp_substs in
  Util.IntSet.cardinal ids = List.length fp_substs

let fp_subst_lookup_or ~default:default narrow fp assignments =
  CCList.find_pred (function {param; expr } -> param = fp) assignments
  |> CCOpt.map_or ~default (function {param; expr } -> narrow expr)

let fp_subst_lookup_or_op ~default:op =
  let narrow = function
    | EO_expr e ->
      implementation_error ~where "Expecting op arg, not expression!"
    | EO_op_arg {location; level; argument } ->
      argument
  in
  fp_subst_lookup_or ~default:op narrow

let fp_subst_lookup_or_expr ~default:op =
  let narrow = function
    | EO_expr e -> e
    | EO_op_arg {location; level; argument } ->
      implementation_error ~where "Expecting expression, not op arg!"
  in
  fp_subst_lookup_or ~default:op narrow


let wrap_expr_in_subst_stack =
  (* TODO: fix location and level, check order of substs applied *)
  let location = Commons.mkDummyLocation in
  let level = None in
  List.fold_left (fun body -> function
      | FP_subst substs ->
        Constr.E.fp_subst_in {location; level; substs; body;  }
      | OP_subst (instantiated_from, instantiated_into, substs) ->
        Constr.E.subst_in { location; level; substs; body;
                            instantiated_from; instantiated_into; }
    )

(* a fold which resets the substitution chain to the given one after each call.
   the desired effect is that an updated termdb is correctly passed around,
   while updates to the substitution chain are independent for each list
   element.
 *)
let fold_resetting_chain_to chain handler initial_acc list =
  let (rlist, acc) =
    List.fold_left
      (function (bss, acc_) ->
       fun bs ->
         let result, acc2 = handler acc_ bs in
         (result :: bss, reset_sub_stack acc2 chain)
      ) ([], initial_acc) list
  in
  ( List.rev rlist, reset_sub_stack acc chain)

let remove_fps_from_chain fps =
  List.map (function
      | FP_subst sub ->
        let new_sub = List.filter (function {param; expr} ->
            List.mem param fps) sub in
        FP_subst new_sub
      | OP_subst (from_module, into_module, insts) ->
        OP_subst (from_module, into_module, insts))


let remove_insts_from_chain rops =
  List.map (function
      | FP_subst sub ->
        FP_subst sub
      | OP_subst (from_module, into_module, insts) ->
        let new_insts =
          List.filter (function {op; expr; next} ->
              List.mem op rops) insts in
        OP_subst (from_module, into_module, new_insts))


let rec apply_fpsubst fp = function
  | {param; expr} :: _ when fp = param -> Some expr
  | _ :: rest -> apply_fpsubst fp rest
  | [] -> None

let is_leibniz_op tdb =
  let check_params = List.fold_left
      (fun acc -> function (_, leibniz) -> acc && leibniz)
      true
  in
  function
  | FMOTA_formal_param fp ->
    (* the leibniz property of a formal parameter depends what it is replaced
       by. right now, we aproximate and assume every parameter with arguments
       is non-leibniz.
    *)
    let fpi = Deref.formal_param tdb fp in
    if fpi.arity = 0 then true else false
  | FMOTA_op_decl _ ->
    (* only declared constants can have arguments. they are always leibniz. *)
    true
  | FMOTA_op_def (O_module_instance mi) ->
    (* It's unclear when this occurs at all *)
    failwith "TODO"
  | FMOTA_op_def (O_user_defined_op uop) ->
    let opdi = Deref.user_defined_op tdb uop in
    check_params opdi.params
  | FMOTA_op_def (O_builtin_op bop) ->
    let opdi = Deref.builtin_op tdb bop in
    check_params opdi.params
  | FMOTA_op_def (O_thm_def _)
  | FMOTA_op_def (O_assume_def _) ->
    (* theorems and assumptions don't have arguments *)
    true
  | FMOTA_ap_subst_in aps ->
    failwith "TODO"
  | FMOTA_lambda lambda ->
    check_params lambda.params

type node_type =
  | Expr of expr * ap_subst_in list
  | AssumeProve of assume_prove * ap_subst_in list

(* returns the sequence of instantiations and the final expr / assume prove *)
let node_type =
  let rec aux acc = function
    | N_expr e -> Expr (e, acc)
    | N_assume_prove ap -> AssumeProve (ap, acc)
    | N_ap_subst_in aps -> aux (aps::acc) aps.body
  in aux []


class ['a] normalize_explicit_substs = object(self)
  inherit ['a] EMap.expr_map

  method expr ({term_db; current_chain; unfolded_uops; _ } as acc) = function
    (* unhandled statements *)
    | E_at _ -> failwith "unhandled at statement"
    | E_label _ -> failwith "unhandled label statement"
    | E_let_in {location; level; op_defs; body} ->
      failwith "unhandled let in statement"
    (* constants *)
    | E_decimal _
    | E_string _
    | E_numeral _ as exp ->
      EMap.return acc exp
    (* unfolding a (recursive) definition *)
    | E_op_appl {location; level;
                 operator = FMOTA_op_def (O_user_defined_op (UOP_ref id as uop));
                 operands = []}
      when within_recursion_limit uop acc ->
      (* increase recursive unfolding counter *)
      let acc0 = inc_id_counter id acc in
      let uopi = Deref.user_defined_op term_db uop in
      self#expr acc0 uopi.body
    (* use of an (instantiated) theorem definition inside an expression *)
    | E_op_appl {location; level;
                 operator = FMOTA_op_def (O_thm_def (TDef_ref id as td)) ;
                 operands = []}
      when IntMap.mem id unfolded_uops ->
      let tdi = Deref.theorem_def term_db td in (
        match node_type tdi.body with
        | Expr (e, substs) ->
          (* create new OP_subst elements for the chain *)
          let new_chain = CCList.fold_left
              (fun rest_substs ->
                 function ({location; level; substs; body;
                            instantiated_from; instantiated_into; }
                           : ap_subst_in)  ->
                   let ops =
                     OP_subst (instantiated_from, instantiated_into, substs)
                   in
                   ops  :: rest_substs )
              current_chain substs in
          let acc0 = update_sub_stack new_chain acc in
          (* handle the inner expression with the new stack *)
          self#expr acc0 e
        | AssumeProve _ ->
          implementation_error ~where
            "trying to unfold an assume prove definition into an expression."
      )
    (* application without arguments *)
    | E_op_appl {location; level; operator; operands = []} ->
      (* operator without arguments *)
      let operator, acc0 = self#operator acc operator in
      E_op_appl {location; level; operator; operands = []}  |> EMap.return acc0
    (* application to a leibniz oparator *)
    | E_op_appl {location; level; operator; operands}
      when is_leibniz_op term_db operator ->
      (* safe first order operator *)
      (* TODO: unfold definitions *)
      let chain = get_sub_stack acc in
      let operator, acc0 = self#operator acc operator in
      (* we need to apply the original substitution to every argument,
         not the modified one from the accumulator *)
      let iacc0 = reset_sub_stack acc0 chain in
      let operands, acc1 =
        fold_resetting_chain_to chain self#expr_or_op_arg iacc0 operands
      in
      let e = E_op_appl {location; level; operator; operands } in
      EMap.return acc1 e
    | exp -> (
      match current_chain with
      | [] ->
        (* nothing to do *)
        EMap.return acc exp
      | (OP_subst (from_module, into_module, op_subst)) :: rest_chain ->
        self#expr_op_subst acc from_module op_subst rest_chain exp
      | (FP_subst fp_subst) :: rest_chain ->
        self#expr_fp_subst acc fp_subst rest_chain exp
    )


  method private expr_op_subst acc mule op_subst rest_chain expr =
    failwith "TODO"

  method private expr_fp_subst acc fp_subst rest_chain =
    function
    (* application and abstraction *)
    | E_op_appl {location; level;
                 operator = FMOTA_formal_param fp; operands = []} as e ->
      let expr = fp_subst_lookup_or_expr ~default:e fp fp_subst in
      (* recurse on remaining stack of substs *)
      let acc0 = update_sub_stack rest_chain acc in
      self#expr acc0 expr
    | E_op_appl {location; level; operator; operands = []} ->
      (* operator without arguments, no fp operator *)
      let operator, acc0 = self#operator acc operator in
      E_op_appl {location; level; operator; operands = []} |> EMap.return acc0
    | E_op_appl {location; level; operator; operands} as expr
      (* when not (is_leibniz_op term_db operator) *) ->
      (* unsafe first order operator *)
      let e = wrap_expr_in_subst_stack expr ((FP_subst fp_subst) :: rest_chain) in
      EMap.return acc e
    | E_binder {location; level; operator; operand; bound_symbols } ->
      let bound_fps = List.fold_left (fun list -> function
          | B_bounded_bound_symbol {params; _} ->
            List.append params list
          | B_unbounded_bound_symbol {param; _} ->
            param::list
        ) [] bound_symbols in
      let chain = get_sub_stack acc in
      let new_chain = remove_fps_from_chain bound_fps chain in
      (* TODO: rename if bound fp in range *)
      let operator, acc0 = self#operator acc operator in
      (* we need to apply the original substitution to every argument,
         not the modified one from the accumulator *)
      let iacc0 = reset_sub_stack acc0 chain in
      let operand, acc1 =  self#expr_or_op_arg iacc0 operand in
      let iacc1 = reset_sub_stack acc1 chain in
      let bounded_symbols, acc2 =
        fold_resetting_chain_to chain
          self#bound_symbol iacc1 bound_symbols
      in
      let e = E_binder {location; level; operator; operand; bound_symbols } in
      EMap.return acc2 e
    (* conversion of explicit substs into the stack *)
    | E_subst_in {location; level; substs; body;
                  instantiated_from; instantiated_into; } ->
      (* convert instantiation into param *)
      let new_head = OP_subst (instantiated_from, instantiated_into, substs) in
      let new_chain = new_head :: (FP_subst fp_subst) :: rest_chain in
      let acc0 = update_sub_stack new_chain acc in
      (* recurse *)
      self#expr acc0 body
    | E_fp_subst_in { location; level; substs; body; } ->
      let new_rest_chain = (FP_subst fp_subst) :: rest_chain in
      let new_chain = (FP_subst substs) :: new_rest_chain in
      let acc0 = update_sub_stack new_chain acc in
      (* recurse, into expr fp case *)
      self#expr_fp_subst acc0 substs new_rest_chain body
    | E_at _ | E_decimal _ | E_label _ | E_let_in _| E_numeral _| E_string _ ->
      implementation_error ~where "This case should have been covered further outside!"

  method operator acc op =
    match get_sub_stack acc with
    | [] ->
      (* nothing to do *)
        EMap.return acc op
    | (OP_subst (from_module, into_module, op_subst)) :: rest_chain ->
      self#operator_op_subst acc from_module op_subst rest_chain op
    | (FP_subst fp_subst) :: rest_chain ->
      self#operator_fp_subst acc fp_subst rest_chain op


  method private operator_fp_subst acc fp_subst rest_chain =
    let { term_db; current_chain; _ } = acc in
    function
    | (FMOTA_formal_param formal_param) as op ->
      let op_ = fp_subst_lookup_or_op ~default:op formal_param fp_subst in
      let acc1 = update_sub_stack rest_chain acc in
      (* recurse remaining chains *)
      self#operator acc1 op_
    | (FMOTA_op_decl _) as op ->
      let acc1 = update_sub_stack rest_chain acc in
      EMap.return acc1 op
    | FMOTA_ap_subst_in {location; level; substs; body;
                         instantiated_from; instantiated_into; } ->
      (*
      let new_head = OP_subst (instantiated_from, instantiated_into, substs) in
      let acc1 = update_sub_stack
          (new_head :: (FP_subst fp_subst) :: rest_chain) acc in
      self#node acc1 body
      *)
      implementation_error ~where
        "the ap_subst_in operator needs to be handled within expr"
    | FMOTA_lambda lambda ->
      failwith "TODO"
    | FMOTA_op_def op_def ->
      failwith "TODO"

  method private operator_op_subst acc =
    failwith "TODO"

  method proof _ = unsupported ~where "proof"
  method assume _ = unsupported ~where "assume"
  method theorem _ = unsupported ~where "theorem"
  method statement _ = unsupported ~where "statement"
  method use_or_hide _ = unsupported ~where "use or hide"
  method step _ = unsupported ~where "step"
  method def_step _ = unsupported ~where "def step"
  method label _ = unsupported ~where "label"
  method entry _ = unsupported ~where "entry"
  method context _ = unsupported ~where "context"
  method mule _ = unsupported ~where "module"
  method mule_entry _ = unsupported ~where "module"

end
