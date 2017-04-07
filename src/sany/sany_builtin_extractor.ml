open Sany_ds
open Commons

type eacc = (int * Expr_ds.builtin_op) list * Sany_ds.entry list
let builtins (b, _) = b
let entries (_, e) = e

class extractor = object(self)
  inherit [eacc] Sany_visitor.visitor as super

  method entry acc { uid; reference } = match reference with
    | E_builtin_op {location; level; name; arity; params } ->
      let params = List.map ( fun param ->
          let (x,y) = param in
          (Sany_expr.convert_formal_param (entries acc) x, y)
        )  params in
      let op = { Expr_ds.level; name; arity; params; } in
      ((uid, op) :: (builtins acc), entries acc)
    | _ -> super#entry acc { uid; reference }

end

let extractor_instance = new extractor
let extract_from_context c =
  extractor_instance#context ([], c.entries) c |> builtins
