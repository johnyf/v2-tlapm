open Any_expr
open List
open Util
open Expr_ds
open Expr_map
open Expr_utils
open Expr_dereference

module Subst =
struct
  (** substitution of formal parameter by an expression *)
  type subst = Subst of formal_param * expr
  type substs = subst list


  let domain = map (function | Subst (x, y) -> x)
  let range = flat_map free_variables

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
      FP { location; level;
           name = find_name fbdef_names 0; arity }
(*
    let remove_from_subst fps =
      let remove_from_subst_ fps =
        fold_left (fun subs ->
                   function Subst (fp, expr) ->
                  )
      in remove_from_subst_ (map dereference_formal_param fps)
 *)

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

type 'a subst_acc = {
  term_db : term_db;
  substs   : Subst.substs;
  bound_context : bound_symbol list;
  subclass_acc : 'a;
}


class ['a] expr_substitution = object(self)
  inherit ['a subst_acc] expr_map as super

  method expr acc = function
    | E_op_appl { location; level; operator = FMOTA_formal_param fp; operands }  ->
      let sacc = get_acc acc in
      begin
        match Subst.find_subst sacc.term_db fp sacc.substs with
        | None ->
          let extract = self#get_id_extractor in
          let operadnds, acc0 =
            unpack_fold extract#expr_or_op_arg self#expr_or_op_arg acc operands in
          acc0
        | Some expr ->
          acc
      end;
      acc
    | E_op_appl { location; level; operator; operands }  ->
      (**)
      acc
    | E_at x        -> self#at acc x
    | E_label x     -> self#label acc x
    | E_subst_in x  -> self#subst_in acc x
    | E_binder x    -> self#binder acc x
    | E_lambda x    -> self#lambda acc x
    (* can not contain formal parameters *)
    | E_decimal x   -> self#decimal acc x
    | E_string x    -> self#strng acc x
    | E_numeral x   -> self#numeral acc x
    | E_let_in x    -> self#let_in acc x


  method binder acc { location; level; operator; operand; bound_symbols } =
    let sacc = get_acc acc in
    let bound_context = append sacc.bound_context bound_symbols in
    let acc0 = set_acc acc { sacc with bound_context } in
    acc0

  method context acc { entries; modules } =
    let inner_acc = get_acc acc in
    let acc0 = set_acc acc { inner_acc with term_db = entries } in
    fold_left self#mule acc0 modules

end
