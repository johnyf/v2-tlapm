open Commons
open Expr_ds
open Any_expr
open Expr_dereference
open Util

module EMap = Expr_map2


class correct_lambda = object(self)
  inherit [term_db * user_defined_op_ IntMap.t] EMap.expr_map as super

  method private lambda_of_uop acc {id; location; level; name; arity;
                                body; params; recursive } =
    let body, acc2 = self#expr acc body in
    let fparams, leibniz = List.split params in
    let rfparams, acc3 = EMap.fold self#formal_param acc2 fparams in
    let params = List.combine rfparams leibniz in
    let lambda = {location; level; arity; body; params} in
    EMap.return acc3 lambda

  method expr_or_op_arg acc = function
    | EO_op_arg ({ location = location;
                   level = level;
                   argument = FMOTA_op_def (O_user_defined_op (UOP_ref id))
                 }  ) ->
      let tdb, map = acc in
      let acc0, argument = (
        (* replace argument by lambda if necessary *)
        match IntMap.get id map with
        | Some uop ->
          let lambda, acc_ = self#lambda_of_uop acc uop in
          (acc_, FMOTA_lambda lambda)
        | None ->
          let uop, acc1 = super#user_defined_op acc (UOP_ref id) in
          (acc1, FMOTA_op_def (O_user_defined_op uop))
      )
      in
      let eo = { location; level; argument; } in
      EMap.return acc0 (EO_op_arg eo)
    | ((EO_op_arg _ ) as eo)
    | ((EO_expr _ ) as eo) ->
      super#expr_or_op_arg acc eo

  method context (tdb, _) { root_module; entries; modules; } =
    (* prepare map of definitions named lambda *)
    let map = List.fold_left
        (fun m -> function
           | (id, UOP_entry e) when e.name = "LAMBDA" ->
             (* remark: LAMBDA is a keyword, there are no real user
                 definitions called like that *)
             IntMap.add id e m
           | _ ->
             m
        )
        IntMap.empty entries
    in
    let ids = IntMap.keys map |> IntSet.of_seq in
    let acc1 = (tdb, map) in
    let acc2, entries = List.fold_left
        (function (acc_, es) ->
         function (id, e) as entry ->
         match Util.IntSet.mem id ids with
         | true -> (* skip lambdas *)
           (acc_, es)
         | false -> (* convert non-lambda entries *)
           let entry, acc_new = self#entry acc_ entry in
           (acc_new, entry :: es)
        )
        (acc1, [])
        entries |> function (x,y) -> (x, List.rev y)
     in
    let context = { root_module; entries; modules } in
    EMap.return acc2 context
end

let instance = new correct_lambda

let correct_lambda_context context =
  let acc = (context.entries, IntMap.empty) in
  let c, macc = instance#context acc context in
  c
