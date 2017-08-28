open List
open Commons
open Expr_ds
open Expr_builtins
open Expr_dereference
open Expr_visitor
open Expr_formatter
open Expr_utils
open Expr_termdb_utils
open Util

module Constr = struct
  let maxlevel l1 l2 = match (l1,l2) with
    | Some l1, Some l2 ->
      Some (max l1 l2)
    | Some l1, None ->
      Some l1
    | None, Some l2 ->
      Some l2
    | None, None ->
      None

  let apply ~location ~level operator operands =
    { location; level; operator; operands; }

  let const_unop builtin ~term_db:tdb ~location
      (op1:expr_or_op_arg) =
    let level = level_of_expr_or_op_arg op1 in
    let and_op = Builtin.get tdb builtin in
    let operator = FMOTA_op_def (O_builtin_op and_op) in
    apply ~location ~level operator [op1]

  let const_binop builtin ~term_db:tdb ~location
      (op1:expr_or_op_arg) (op2:expr_or_op_arg) =
    let level = maxlevel
        (level_of_expr_or_op_arg op1)
        (level_of_expr_or_op_arg op2) in
    let and_op = Builtin.get tdb builtin in
    let operator = FMOTA_op_def (O_builtin_op and_op) in
    apply ~location ~level operator [op1; op2]

  let neg       = const_unop Builtin.NOT
  let conj      = const_binop Builtin.AND
  let disj      = const_binop Builtin.OR
  let impl      = const_binop Builtin.IMPLIES
  let eqality   = const_binop Builtin.EQ
  let nequality = const_binop Builtin.NEQ

  let fp ~term_db:tdb ~location:location ~level:level name arity =
    let fp = ({id = -1; location; level; name; arity;} : formal_param_) in
    mkref_formal_param tdb fp

  let op_dec ~term_db:tdb ~location:location ~level:level name arity kind =
    let opd = {id = -1; location; level; name; arity; kind} in
    mkref_opdec tdb opd

  let constant ~term_db:tdb ~location:location name arity =
    op_dec ~term_db:tdb ~location ~level:(Some ConstantLevel)
      name arity ConstantDecl

  let variable ~term_db:tdb ~location:location name arity =
    op_dec ~term_db:tdb ~location ~level:(Some StateLevel)
      name arity VariableDecl

  let op_definition ~recursive:recursive ~term_db:tdb ~location:location
      name body params =
    let level = List.fold_left
        (fun l -> function (fp, _) ->
            let fpi = Deref.formal_param tdb fp in
            max l fpi.level)
        (level_of_expr body) params in
    let arity = List.length params in
    let opd = {id = -1; location; level; name; arity;
               body; params; recursive} in
    mkref_user_defined_op tdb opd

  let uop_def = op_definition ~recursive:false
  let rec_uop_def = op_definition ~recursive:true

  let quantifier ~term_db:tdb ~location:location ~level:level q bound_symbols operand =
    let qop = Builtin.get tdb q in
    let bounded_vars = List.filter
        (function
          | B_bounded_bound_symbol _ -> true;
          | B_unbounded_bound_symbol _ -> false;
        ) bound_symbols in
    match q with
    | Builtin.BEXISTS
    | Builtin.BFORALL when bound_symbols = [] ->
      { location; level;
        operator = FMOTA_op_def (O_builtin_op qop);
        operand;
        bound_symbols;
      }
    | Builtin.BEXISTS
    | Builtin.BFORALL (* otherwise *) ->
      let msg = CCFormat.sprintf
          "Unbounded quantifier %a requires unbounded variables symbols %a"
          Builtin.pp q
          (CCFormat.list (fun f bs -> CCFormat.fprintf f "" ))
          bounded_vars
      in
      failwith msg
    | _ ->
      let msg = CCFormat.sprintf "%a is not a quantifier!" Builtin.pp q in
      failwith msg

  let bquantifier ~term_db:tdb ~location:location ~level:level q bound_symbols operand =
    let qop = Builtin.get tdb q in
    let unbounded_vars = List.filter
        (function
          | B_bounded_bound_symbol _ -> false;
          | B_unbounded_bound_symbol _ -> true;
        ) bound_symbols in
    match q with
    | Builtin.BEXISTS
    | Builtin.BFORALL when bound_symbols = [] ->
      { location; level;
        operator = FMOTA_op_def (O_builtin_op qop);
        operand;
        bound_symbols;
      }
    | Builtin.BEXISTS
    | Builtin.BFORALL (* otherwise *) ->
      let msg = CCFormat.sprintf
          "Bounded quantifier %a requires bounded variables symbols %a"
          Builtin.pp q
          (CCFormat.list (fun f bs -> CCFormat.fprintf f "" ))
          unbounded_vars
      in
      failwith msg
    | _ ->
      let msg = CCFormat.sprintf "%a is not a quantifier!" Builtin.pp q in
      failwith msg

  let guard_of_binder ~term_db:tdb { location; level; operator;
                            operand; bound_symbols; } =
    let e_quants = List.map (Builtin.get tdb)
        [Builtin.EXISTS; Builtin.BEXISTS;
         Builtin.FORALL; Builtin.BFORALL;] in
    match operator with
    | FMOTA_op_def (O_builtin_op bop)
      when List.mem bop e_quants ->
      List.fold_left (fun aux ->
          function
          | B_bounded_bound_symbol {params; tuple; domain; } ->
            let location =  location_of_expr domain in
            let level = List.fold_left
                (fun l fp ->
                   let fpi = Deref.formal_param tdb fp in
                   max l fpi.level
                ) (level_of_expr domain)
                params
            in
            let guard = apply ~location in
            aux
          | B_unbounded_bound_symbol ubs ->
            aux
        ) [] bound_symbols
    | _ ->
      let msg =
        CCFormat.sprintf "Can't extract guards of binder operator %a!"
          (fmt_operator tdb) operator
      in failwith msg
end
