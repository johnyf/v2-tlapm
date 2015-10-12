open Commons
open Expr_ds
open Expr_map
open Any_expr

class ['a] correct_lambda = object(self)
inherit ['a] expr_map as super

method expr acc = function
  | other -> super#expr acc other


(* *)
method expr_or_op_arg acc eo = match eo with
| EO_op_arg ({ location; level; argument  } as eoi ) ->
   ( match argument with
     | FMOTA_op_def
       (OPDef
        (O_user_defined_op
         (UOP ({location; level; name="LAMBDA"; arity; body; params;} as uop))) ) ->
        (* remark: LAMBDA is a keyword, there are no real user
           definitions called like that *)
        (* Printf.printf "replacing a lambda at %s %s!\n"
                        (Commons.format_location eoi.location) uop.name; *)
        let acc1 = self#level acc level in
        let acc2 = self#expr acc1 body in
        let fparams, leibniz = List.split params in
        let ie = self#get_id_extractor in
        let rfparams, acc3 = unpack_fold ie#formal_param
                                         self#formal_param acc2 fparams in
        let params = List.combine rfparams leibniz in
        let r = E_lambda ({level; arity; body; params}) in
        set_anyexpr acc (Any_expr_or_op_arg (EO_expr r))
     (* i've seen only inline UOP lambdas so far, but there might be references *)
     (* TODO: check references*)
     | _ -> super#expr_or_op_arg acc eo
   )
| _ -> super#expr_or_op_arg acc eo
end
