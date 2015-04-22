open Sany_ds
open Commons

class extractor = object(self)
  inherit [(int * Expr_ds.builtin_op) list] Sany_visitor.visitor as super

  method entry acc { uid; reference } = match reference with
    | FMOTA_op_def
      (OPDef (O_builtin_op (BOP {location; level; name; arity; params }))) ->
       let params = List.map ( fun param ->
			       let (x,y) = param in
			       (Sany_expr.convert_formal_param x, y)
			     )  params in
       let op = { Expr_ds.level; name; arity; params; } in
       (uid, op) :: acc
    | _ -> super#entry acc { uid; reference }

end

let extractor_instance = new extractor
let extract_from_context c = extractor_instance#context [] c
