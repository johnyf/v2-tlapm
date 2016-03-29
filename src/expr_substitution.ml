open List
open Util
open Expr_ds
open Expr_map
open Expr_utils
open Expr_dereference

module Subst = struct
    (** substitution of formal parameter by an expression *)
    type subst = Subst of formal_param * expr
    type substs = subst list


    let domain = map (function | Subst (x, y) -> x)
    let range = flat_map free_variables

    let rec rename term_db ?free:(free=[]) ?bound:(bound=[]) ?defs:(defs=[]) = function
      | (FP_ref x) as fp ->
         let fpi = dereference_formal_param term_db fp in
         rename term_db ~free ~bound (FP fpi)
      | FP { location; level; name; arity; } ->
         let free_names =
           map (fun x ->
                let opdeci = dereference_op_decl term_db x in
                opdeci.name
               ) free
         in
         let bound_names =
           map (fun x ->
                let fpi = dereference_formal_param term_db x in
                fpi.name
               ) bound
         in
         let rec find_name f b n =
           let new_name = name ^ (string_of_int n) in
           if mem new_name bound_names then
             find_name f b (n+1)
           else
             if mem new_name free_names then
               find_name f b (n+1)
             else
               new_name
         in
         FP { location; level; name = find_name free_names bound_names 0; arity }
  end

type 'a subst_acc = {
    term_db : term_db;
    substs   : Subst.substs;
    bound_context : bound_symbol list;
    subclass_acc : 'a;
  }

                      
class ['a] expr_substitution = object
  inherit ['a subst_acc] expr_map as self
                                       
  method context acc { entries; modules } =
    let inner_acc = get_acc acc in
    let acc0 = set_acc acc { inner_acc with term_db = entries } in
    fold_left self#mule acc0 modules

end
