open Commons
open Expr_ds
open Expr_map
open Any_expr
open Expr_dereference
open Util

class correct_lambda = object(self)
  inherit [term_db * user_defined_op_ IntMap.t] expr_map as super

  method private lambda_of_uop acc {id; location; level; name; arity;
                                body; params; recursive } =
    let acc2 = self#expr acc body in
    let body = self#get_macc_extractor#expr acc2 in
    let fparams, leibniz = List.split params in
    let ie = self#get_id_extractor in
    let rfparams, acc3 = unpack_fold ie#formal_param
        self#formal_param acc2 fparams in
    let params = List.combine rfparams leibniz in
    let lambda = Any_lambda {location; level; arity; body; params} in
    set_anyexpr acc3 lambda

  method expr_or_op_arg acc = function
    | EO_op_arg ({ location = location;
                   level = level;
                   argument = FMOTA_op_def (O_user_defined_op (UOP_ref id))
                 }  ) ->
      let tdb, map = get_acc acc in
      let acc0, argument = (
        (* replace argument by lambda if necessary *)
        match IntMap.get id map with
        | Some uop ->
          let acc_ = self#lambda_of_uop acc uop in
          let lambda = self#get_macc_extractor#lambda acc_ in
          (acc_, FMOTA_lambda lambda)
        | None ->
          let me = (self#get_macc_extractor)#user_defined_op in
          let acc1 = super#user_defined_op acc (UOP_ref id) in
          (acc1, FMOTA_op_def (O_user_defined_op (me acc1)))
      )
      in
      let eo = { location; level; argument; } in
      set_anyexpr acc0 (Any_expr_or_op_arg (EO_op_arg eo))
    | ((EO_op_arg _ ) as eo)
    | ((EO_expr _ ) as eo) ->
      super#expr_or_op_arg acc eo

  method context acc { root_module; entries; modules; } =
    let ee = self#get_macc_extractor#entry in
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
    let acc1 = acc |> function (a, (t,_)) -> (a,(t,map)) in
    let acc2, entries = List.fold_left
        (function (acc_, es) ->
         function (id, e) as entry ->
         match Util.IntSet.mem id ids with
         | true -> (* skip lambdas *)
           (acc_, es)
         | false -> (* convert non-lambda entries *)
           let acc_new = self#entry acc_ entry in
           (acc_new, (ee acc_new) :: es)
        )
        (acc1, [])
        entries |> function (x,y) -> (x, List.rev y)
     in
    let context = { root_module; entries; modules } in
    set_anyexpr acc2 (Any_context context)
end

let instance = new correct_lambda

let correct_lambda_context context =
  let me = instance#get_macc_extractor in
  let acc = (Nothing, (context.entries, IntMap.empty)) in
  let macc = instance#context acc context in
  let c = me#context macc in
  c
