open Expr_ds
open Expr_constructors
open Expr_map
open Expr_dereference
open Expr_substitution

type sub_stack =
  | FP_subst of Expr_ds.fp_assignment list
  | OP_subst of Expr_ds.mule * Expr_ds.mule * Expr_ds.instantiation list

type 'a nacc = NAcc of term_db * sub_stack list * 'a

let get_termdb (NAcc (tdb,_,_)) = tdb
let get_sub_stack (NAcc (_,stack,_)) = stack
let nacc_update_term_db  tdb (NAcc (_, stack, pl)) = NAcc (tdb,stack,pl)
let nacc_update_sub_stack  stack (NAcc (tdb, _, pl)) = NAcc (tdb,stack,pl)

let reset_sub_stack acc chain =
  get_acc acc
  |> nacc_update_sub_stack chain
  |> set_acc acc

let unsupported s =
  let msg = CCFormat.sprintf "Can't : %s" s in
  failwith msg

let check_fpsubst fp_substs =
  (* checks if all formal parameter references are unique *)
  let ids = List.fold_left
      (fun set -> function | {param = FP_ref id; expr;} ->
          Util.IntSet.add id set)
      Util.IntSet.empty fp_substs in
  Util.IntSet.cardinal ids = List.length fp_substs

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
let fold_resetting_chain_to chain handler extractor initial_acc list =
  let (acc, rlist) =
    List.fold_left
      (function (acc_, bss) ->
       fun bs ->
         handler acc_ bs
         |> fun a -> (reset_sub_stack a chain, extractor a :: bss)
      ) (initial_acc, []) list
  in
  (reset_sub_stack acc chain, List.rev rlist)

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

class ['a] normalize_explicit_substs = object(self)
  inherit ['a] expr_map

  method expr acc exp =
    let subst_chain = get_acc acc |> get_sub_stack in
    match subst_chain with
    | [] ->
      (* nothing to do *)
        set_anyexpr acc (Any_expr exp)
    | (OP_subst (from_module, into_module, op_subst)) :: rest_chain ->
      self#expr_op_subst acc from_module op_subst rest_chain exp
    | (FP_subst fp_subst) :: rest_chain ->
      self#expr_fp_subst acc fp_subst rest_chain exp


  method private expr_op_subst acc mule op_subst rest_chain expr =
    failwith "TODO"

  method private expr_fp_subst acc fp_subst rest_chain =
    let me = self#get_macc_extractor in
    let set_expr acc e = set_anyexpr acc (Any_expr e) in
    let term_db = get_acc acc |> get_termdb in
    function
    (* unhandled statements *)
    | E_at _ -> failwith "unhandled at statement"
    | E_label _ -> failwith "unhandled label statement"
    | E_let_in {location; level; op_defs; body} ->
      failwith "unhandled let in statement"
    (* constants *)
    | E_decimal _
    | E_string _
    | E_numeral _ as exp ->
      set_anyexpr acc (Any_expr exp)
    (* application and abstraction *)
    | E_op_appl {location; level;
                 operator = FMOTA_formal_param fp; operands = []} as e ->
      (* operator without arguments, direct fp application *)
      let expr = match apply_fpsubst fp fp_subst with
        | Some (EO_expr e) -> e (* subst applied *)
        | None -> e             (* fp is not in subst *)
        | Some (EO_op_arg _) ->
          failwith "Can't substitute an op_arg for an expression!"
      in
      (* recurse on remaining stack of substs *)
      let nacc0 = get_acc acc |> nacc_update_sub_stack rest_chain in
      let acc0 = set_acc acc nacc0 in
      self#expr acc0 expr
    | E_op_appl {location; level; operator; operands = []} ->
      (* operator without arguments, no fp operator *)
      let acc0 = self#operator acc operator in
      E_op_appl {location; level; operator; operands = []} |> set_expr acc0
    | E_op_appl {location; level; operator; operands = []} ->
      (* operator without arguments *)
      let acc0 = self#operator acc operator in
      E_op_appl {location; level; operator; operands = []}  |> set_expr acc0
    | E_op_appl {location; level; operator; operands}
      when is_leibniz_op term_db operator ->
      (* safe first order operator *)
      let chain = get_acc acc |> get_sub_stack in
      let acc0 = self#operator acc operator in
      (* we need to apply the original substitution to every argument,
         not the modified one from the accumulator *)
      let iacc0 = reset_sub_stack acc0 chain in
      let acc1, operands =
        fold_resetting_chain_to
          chain self#expr_or_op_arg me#expr_or_op_arg
          iacc0 operands
      (*
        List.fold_left (fun (facc,ops) oa ->
          let iacc = get_acc facc
                     |> nacc_update_sub_stack chain
                     |> set_acc facc in
          let iacc0 = self#expr_or_op_arg iacc oa in
          (iacc0, me#expr_or_op_arg iacc0 :: ops)
        ) (acc0, []) operands
      *)
      in
      let operator = me#operator acc0 in
      let e = E_op_appl {location; level; operator; operands } in
      set_anyexpr acc1 (Any_expr e)
    | E_op_appl {location; level; operator; operands} as expr
      (* when not (is_leibniz_op term_db operator) *) ->
      (* unsafe first order operator *)
      let e = wrap_expr_in_subst_stack expr ((FP_subst fp_subst) :: rest_chain) in
      set_anyexpr acc (Any_expr e)
    | E_binder {location; level; operator; operand; bound_symbols } ->
      let bound_fps = List.fold_left (fun list -> function
          | B_bounded_bound_symbol {params; _} ->
            List.append params list
          | B_unbounded_bound_symbol {param; _} ->
            param::list
        ) [] bound_symbols in
      let chain = get_acc acc |> get_sub_stack in
      let new_chain = remove_fps_from_chain bound_fps chain
      in
      (* TODO: rename if bound fp in range *)
      let acc0 = self#operator acc operator in
      (* we need to apply the original substitution to every argument,
         not the modified one from the accumulator *)
      let iacc0 = reset_sub_stack acc0 chain in
      let acc1 =  self#expr_or_op_arg iacc0 operand in
      let iacc1 = reset_sub_stack acc0 chain in
      let acc2, bounded_symbols =
        fold_resetting_chain_to chain
          self#bound_symbol me#bound_symbol iacc1 bound_symbols
      in
      let operator = me#operator acc0 in
      let operand =  me#expr_or_op_arg iacc0 in
      let e = E_binder {location; level; operator; operand; bound_symbols } in
      set_anyexpr acc1 (Any_expr e)
    (* conversion of explicit substs into the stack *)
    | E_subst_in {location; level; substs; body;
                  instantiated_from; instantiated_into; } ->
      (* convert instantiation into param *)
      let new_head = OP_subst (instantiated_from, instantiated_into, substs) in
      let new_chain = new_head :: (FP_subst fp_subst) :: rest_chain in
      let inner_acc = get_acc acc |> nacc_update_sub_stack new_chain in
      let acc0 = set_acc acc inner_acc in
      (* recurse *)
      self#expr acc0 body
    | E_fp_subst_in { location; level; substs; body; } ->
      let new_rest_chain = (FP_subst fp_subst) :: rest_chain in
      let new_chain = (FP_subst substs) :: new_rest_chain in
      let inner_acc = get_acc acc |> nacc_update_sub_stack new_chain in
      let acc0 = set_acc acc inner_acc in
      (* recurse, into expr fp case *)
      self#expr_fp_subst acc0 substs new_rest_chain body

  method operator acc =
    let (NAcc (termdb, subst_chain,_)) = Expr_map.get_acc acc in
    function
    | _ -> failwith "TODO"

  method proof _ = unsupported "proof"
  method assume _ = unsupported "assume"
  method theorem _ = unsupported "theorem"
  method statement _ = unsupported "statement"
  method use_or_hide _ = unsupported "use or hide"
  method step _ = unsupported "step"
  method def_step _ = unsupported "def step"
  method label _ = unsupported "label"
  method entry _ = unsupported "entry"
  method context _ = unsupported "context"
  method mule _ = unsupported "module"
  method mule_entry _ = unsupported "module"

end
