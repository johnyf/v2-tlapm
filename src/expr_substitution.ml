open Any_expr
open List
open Util
open Expr_ds
open Expr_map
open Expr_utils
open Expr_dereference
open Expr_termdb_utils

module Subst =
  struct
    (** substitution of formal parameter by an expression *)
    type subst = Subst of formal_param * expr_or_op_arg
    type substs = subst list


    let domain = map (function | Subst (x, y) -> x)
    let range =
      flat_map (function
                 | Subst (_, EO_expr e) -> free_variables e
                 | _ -> [] )
    let bound_range =
      flat_map (function
                 | Subst (_, EO_expr e) -> bound_variables e
                 | _ -> [] )

    let rec rename term_db ?free:(free=[]) ?bound:(bound=[]) ?defs:(defs=[]) =
      function
      | (FP_ref x) as fp ->
         let fpi = dereference_formal_param term_db fp in
         rename term_db ~free ~bound (FP fpi)
      | FP { location; level; name; arity; } ->
         let free_names =
           fold_left (fun x y ->
                let opdeci = dereference_op_decl term_db y in
                opdeci.name :: x
               ) [] free
         in
         let fbound_names =
           fold_left (fun x y ->
                let fpi = dereference_formal_param term_db y in
                fpi.name :: x
               ) free_names bound
         in
         let fbdef_names =
           fold_left
             (fun x ->
              function
              | O_module_instance mi -> x
              | O_builtin_op op -> op.name :: x
              | O_user_defined_op op ->
                 let opi = dereference_user_defined_op term_db op in
                 opi.name :: x
             ) fbound_names defs
         in
         let rec find_name blacklist n =
           let new_name = name ^ (string_of_int n) in
           if mem new_name fbdef_names then
             find_name blacklist (n+1)
           else
             new_name
         in
         let fp_ = { location; level; name = find_name fbdef_names 0; arity } in
         mkref_formal_param term_db fp_

    (* removes formal params in fps from substition domain *)
    let remove_from_subst termdb fps =
      let remove_from_subst_ termdb fps =
        filter (function Subst (fp, expr) ->
                         mem (dereference_formal_param termdb fp) fps
               )
      in remove_from_subst_ termdb (map (dereference_formal_param termdb) fps)

    (* looks for mapping of fp in substs *)
    let find_subst ?cmp:(cmp=(=))  term_db fp =
      let fpi = dereference_formal_param term_db fp in
      fold_left (function
                  | None ->
                     begin
                       function
                       | Subst (v, exp) ->
                          let vi = dereference_formal_param term_db v in
                          if cmp fpi vi then
                            Some exp
                          else
                            None
                     end
                  | Some _ as x ->
                     fun _ -> x
                ) None
  end

(* substitution algorithm *)
type 'a subst_acc = {
    term_db : term_db;
    substs   : Subst.substs;
    bound_context : formal_param list;
    bound_renaming : (formal_param * formal_param) list;
    subclass_acc : 'a;
  }


class ['a] expr_substitution = object(self)
  inherit ['a subst_acc] expr_map as super

  method expr acc = function
    (* formal param replaced by expression *)
    | E_op_appl { location; level;
                  operator = FMOTA_formal_param fp; operands = [] }  ->
       let sacc = get_acc acc in
       begin
         match Subst.find_subst sacc.term_db fp sacc.substs with
         | None ->
            let e = E_op_appl { location; level;
                                operator = FMOTA_formal_param fp;
                                operands = []; } in
            set_anyexpr acc (Any_expr e)
         | Some (EO_expr e) ->
            set_anyexpr acc (Any_expr e)
         | Some (EO_op_arg o) ->
            failwith "Tried to replace an expression by an operator."
       end
    (* formal param replaced by operator *)
    | E_op_appl opappl as e  ->
       super#expr acc e (* handled in self#operator *)
    | E_binder b as e -> super#expr acc e (* handled in self#binder *)
    | E_at x        as e -> super#expr acc e (* TODO: handle *)
    | E_label x     as e -> super#expr acc e (* TODO: handle *)
    | E_subst_in x  as e -> super#expr acc e (* TODO: handle *)
    (* can not contain formal parameters *)
    | E_decimal x   as e -> super#expr acc e
    | E_string x    as e -> super#expr acc e
    | E_numeral x   as e -> super#expr acc e
    | E_let_in x    as e -> super#expr acc e

  method unbounded_bound_symbol acc { param; tuple } =
    let sacc = get_acc acc in
    let free = Subst.range sacc.substs in
    let bound = sacc.bound_context in
    let (rterm_db, rparam) = Subst.rename sacc.term_db ~free ~bound param in
    let (bound_context, bound_renaming) =
      if compare_modulo_deref_formal_param rterm_db param rparam
      then (sacc.bound_context, [])
      else (rparam :: sacc.bound_context, [(param, rparam)])
    in
    let acc0 = set_acc acc { sacc with term_db = rterm_db; bound_context; } in
    let ubs = { param = rparam; tuple; } in
    set_anyexpr acc0 (Any_unbounded_bound_symbol ubs)

  method bounded_bound_symbol acc { params; tuple; domain } =
    (* recurse into domain first, SANY does not allow params
       in the domain *)
    let acc0 = self#expr acc domain in
    let term_db = (get_acc acc0).term_db in
    let parent_acc = (get_acc acc) in
    let bound = parent_acc.bound_context in
    let free = Subst.range parent_acc.substs in
    (* do renaming of symbols, if neccessary *)
    let (rterm_db, rparams_reverse) =
      fold_left (function
                  | (rdb, rps) ->
                     (fun param ->
                      let db, rp =
                        Subst.rename term_db ~free ~bound param
                      in
                      (db, rp::rps)
                     )
                )
                (term_db, []) params
    in
    (* restore parameter order *)
    let rparams = rev rparams_reverse in
    (* create the renaming from old to new symbols *)
    let bound_renaming =
      filter (function | (fpo, fpn) ->
                          compare_modulo_deref_formal_param rterm_db fpo fpn)
             (combine params rparams)
    in
    (* add new symbols to bound context *)
    let bound_context = append parent_acc.bound_context
                               (map (function (_,x) -> x) bound_renaming)
    in
    let acc0 = set_acc acc { parent_acc with term_db = rterm_db;
                             bound_context; bound_renaming; } in
    let ubs = { params = rparams; tuple; domain } in
    set_anyexpr acc0 (Any_bounded_bound_symbol ubs)

  method operator acc = function
    | FMOTA_formal_param fp as op ->
       let sacc = get_acc acc in
       let extract = self#get_id_extractor in
       begin
         match Subst.find_subst sacc.term_db fp sacc.substs with
         | None ->
            set_anyexpr acc (Any_operator op)
         | Some (EO_op_arg { location; level; argument }) ->
            set_anyexpr acc (Any_operator argument)
         | Some (EO_expr e) ->
            failwith "Tried to replace an expression by an operator."
       end
    | FMOTA_module mule             as op -> super#operator acc op
    | FMOTA_op_decl op_decl         as op -> super#operator acc op
    | FMOTA_op_def op_def           as op -> super#operator acc op
    | FMOTA_theorem theorem         as op -> super#operator acc op
    | FMOTA_assume assume           as op -> super#operator acc op
    | FMOTA_ap_subst_in ap_subst_in as op -> super#operator acc op
    | FMOTA_lambda lambda           as op -> super#operator acc op

  method binder acc { location; level; operator; operand; bound_symbols } =
    let sacc = get_acc acc in
    (* process bound symbols to get the renaming *)
    let ie = self#get_id_extractor in
    let bound_symbols, racc =
      unpack_fold ie#bound_symbol self#bound_symbol acc bound_symbols in
    (* apply renaming to operand, the operator is always a built-in and
          doesn't need to be processed *)
    let rsacc = get_acc racc in
    let substs =
      map (function (x,y) ->
                    let argument = FMOTA_formal_param y in
                    let eo = EO_op_arg { location; level; argument } in
                    Subst.Subst (x, eo)
          )
          rsacc.bound_renaming
    in
    let renaming_acc = set_acc racc
                               { sacc with substs; term_db = rsacc.term_db }
    in
    let renamed_op, sacc0 = self#expr_or_op_arg renaming_acc operand in
    (* apply substitution to renamed operator *)
    (* TODO: the renaming assures that we don't need to remove the old
       bound variables from the substitution. Decide if we want to remove them
       anyway.

       Right now, we also do not rename if there is a collision with a
       formal parameter bound in one of the domains in bound_symbols.
     *)
    let substituting_sacc = { sacc with term_db = sacc0.term_db;
                                        bound_context = rsacc.bound_context;
                            } in
    let substituting_acc = (renamed_op, substituting_sacc) in
    let op, sacc1 =
      ie#expr_or_op_arg renamed_op
      |> self#expr_or_op_arg substituting_acc
    in
    (* create binder for return value *)
    let binder = { location; level; operator;
                   operand = ie#expr_or_op_arg op;
                   bound_symbols }
    in
    (* create subst accumulator for passing up *)
    let bsacc = { sacc with term_db = sacc0.term_db;
                            bound_context = rsacc.bound_context } in
    (Any_binder binder, bsacc)

  method user_defined_op acc = function
    | UOP_ref uid as uop ->
       set_anyexpr acc (Any_user_defined_op uop)
    | UOP uopi as uop ->
       (* Don't recurse into the body, any substitution is applied
          to the arguments as soon as the operator is applied.
        *)
       set_anyexpr acc (Any_user_defined_op uop)

  method context acc _ =
    failwith "Can't apply a substitutionto a context."

end
