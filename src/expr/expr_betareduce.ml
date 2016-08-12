open Commons
open Expr_ds
open Expr_map

type 'a beta_acc = context * 'a

let get_context (c,_) = c
let get_acc (_,a) = a

class ['a] beta_reduce =
  object(self)
    inherit ['a beta_acc] expr_map as super

    (*method op_appl acc {location; level; operator; operands} =
      match operator with
      | FMOTA_op_decl x   E_lambda { level; arity; body; params } ->
    *)

  end
