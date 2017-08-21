open Commons
open Expr_ds
open Expr_map
open Any_expr
open Expr_dereference

class correct_lambda = object(self)
  inherit [term_db] expr_map as super

  method expr_or_op_arg acc = function
    | EO_op_arg ({ location = eo_location;
                   level = eo_level;
                   argument = FMOTA_op_def (O_user_defined_op uop)
                 }  ) ->
      let tdb = get_acc acc in
      ( match Deref.user_defined_op tdb uop with
        | {id; location; level; name="LAMBDA"; arity; body; params;} ->
          (* remark: LAMBDA is a keyword, there are no real user
                 definitions called like that *)
          Printf.eprintf "Replacing lambda of id %a\n"
            (fun chan -> function | UOP_ref x -> Printf.fprintf chan "%d" x) uop;
          (* Printf.printf "replacing a lambda at %s %s!\n"
                          (Commons.format_location eoi.location) uop.name; *)
          let acc2 = self#expr acc body in
          let body = self#get_macc_extractor#expr acc2 in
          let fparams, leibniz = List.split params in
          let ie = self#get_id_extractor in
          let rfparams, acc3 = unpack_fold ie#formal_param
              self#formal_param acc2 fparams in
          let params = List.combine rfparams leibniz in
          let argument = FMOTA_lambda ({location; level; arity; body; params}) in
          let oparg = {
            location = eo_location;
            level = eo_level;
            argument;
          } in
          set_anyexpr acc (Any_expr_or_op_arg (EO_op_arg oparg))
        | _ (* normal op def *) ->
          let me = (self#get_macc_extractor)#user_defined_op in
          let acc1 = super#user_defined_op acc uop in
          let argument = FMOTA_op_def (O_user_defined_op (me acc1)) in
          let eo = {
            location = eo_location;
            level = eo_level;
            argument;
          } in
          set_anyexpr acc1 (Any_expr_or_op_arg (EO_op_arg eo))
      )
    | ((EO_op_arg _ ) as eo)
    | ((EO_expr _ ) as eo) ->
      super#expr_or_op_arg acc eo

  method context acc c =
    let acc0 = super#context acc c in
    let me = self#get_macc_extractor#context in
    let { root_module; entries; modules; } = me acc0 in
    let entries = List.filter
        (function | (id, UOP_entry e) -> e.name <> "LAMBDA"
                  | (_, _) -> true
        )
        entries in
    let context = { root_module; entries; modules } in
    set_anyexpr acc0 (Any_context context)
end

let instance = new correct_lambda

let correct_lambda_context context =
  let me = instance#get_macc_extractor in
  let macc = instance#context (Nothing, context.entries) context in
  let c = me#context macc in
  c
