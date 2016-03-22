open List
open Expr_ds
open Expr_map

(** substitution of formal parameter by an expression *)
type subst = Subst of formal_param * expr

type 'a subst_acc = {
    term_db : term_db;
    substs   : subst list;
    bound_context : bound_symbol list;
  }

class ['a] expr_substitution = object
  inherit ['a subst_acc] expr_map as self

  method context acc { term_db; modules } =
    let inner_acc = get_acc acc in
    let acc0 = set_acc acc { inner_acc with term_db = term_db } in
    fold_left self#mule acc0 modules

end
