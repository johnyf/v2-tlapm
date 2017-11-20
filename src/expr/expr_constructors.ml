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
  module OPD = struct
    let mi x = O_module_instance x
    let uop x = O_user_defined_op x
    let bop x = O_builtin_op x
    let thm_def x = O_thm_def x
    let assume_def x = O_assume_def x

    (* accessors for common fields *)
    let level_of tdb = function
      | O_user_defined_op op ->
        (Deref.user_defined_op tdb op).level
      | O_builtin_op op ->
        (Deref.builtin_op tdb op).level
      | O_module_instance op ->
        (Deref.module_instance tdb op).level
      | O_thm_def op ->
        (Deref.theorem_def tdb op).level
      | O_assume_def op ->
        (Deref.assume_def tdb op).level

    let location_of tdb = function
      | O_user_defined_op op ->
        (Deref.user_defined_op tdb op).location
      | O_builtin_op op ->
        Commons.mkDummyLocation
      | O_module_instance op ->
        (Deref.module_instance tdb op).location
      | O_thm_def op ->
        (Deref.theorem_def tdb op).location
      | O_assume_def op ->
        (Deref.assume_def tdb op).location

    let arity_of tdb = function
      | O_user_defined_op op ->
        (Deref.user_defined_op tdb op).arity
      | O_builtin_op op ->
        (Deref.builtin_op tdb op).arity
      | O_module_instance op -> -1 (* TODO: check arity of module instance *)
      | O_thm_def op         ->  0
      | O_assume_def op      ->  0
  end

  module Op = struct
    let formal_param x = FMOTA_formal_param x
    let op_def x = FMOTA_op_def x
    let op_decl x = FMOTA_op_decl x
    let ap_subst_in x = FMOTA_ap_subst_in x
    let fp_subst_in x = FMOTA_ap_subst_in x
    let lamda x = FMOTA_lambda x

    (* accessors for common fields *)
    let level_of tdb = function
      | FMOTA_formal_param op ->
        (Deref.formal_param tdb op).level
      | FMOTA_op_decl op ->
        (Deref.op_decl tdb op).level
      | FMOTA_op_def op ->
        OPD.level_of tdb op
      | FMOTA_ap_subst_in op ->
        op.level
      | FMOTA_lambda op ->
        op.level

    let location_of tdb = function
      | FMOTA_formal_param op ->
        (Deref.formal_param tdb op).location
      | FMOTA_op_decl op ->
        (Deref.op_decl tdb op).location
      | FMOTA_op_def op ->
        OPD.location_of tdb op
      | FMOTA_ap_subst_in op ->
        op.location
      | FMOTA_lambda op ->
        op.location

    let arity_of tdb = function
      | FMOTA_formal_param op ->
        (Deref.formal_param tdb op).arity
      | FMOTA_op_decl op ->
        (Deref.op_decl tdb op).arity
      | FMOTA_op_def op ->
        OPD.arity_of tdb op
      | FMOTA_ap_subst_in op -> -1 (* TODO: check arity of ap_subst_in *)
      | FMOTA_lambda op ->
        op.arity
  end

  module E = struct
    let at x = E_at x
    let decimal x = E_decimal x
    let label x = E_label x
    let let_in x = E_let_in x
    let numeral x = E_numeral x
    let op_appl x = E_op_appl x
    let strng x = E_string x
    let subst_in x = E_subst_in x
    let fp_subst_in x = E_fp_subst_in x
    let binder x = E_binder x

    let level_of = Expr_utils.level_of_expr
    let location_of = Expr_utils.location_of_expr
  end

  module EO = struct
    let expr  x = EO_expr x
    let op_arg x = EO_op_arg x
  end

  let numeral ~location:location value =
    E_numeral { location;
                level = Some ConstantLevel;
                value;
              }

  let decimal ~location:location mantissa exponent =
    E_decimal { location;
                level = Some ConstantLevel;
                mantissa;
                exponent;
              }

  let strng ~location:location value =
    E_string {
      location;
      level = Some ConstantLevel;
      value;
    }

  let maxlevel l1 l2 = match (l1,l2) with
    | Some l1, Some l2 ->
      Some (max l1 l2)
    | Some l1, None ->
      Some l1
    | None, Some l2 ->
      Some l2
    | None, None ->
      None



  let apply ~term_db:term_db ~location ~level operator operands =
    let arity = Op.arity_of term_db operator in
    match List.length operands with
    | n when arity < 0 || arity = n ->
      { location; level; operator; operands; }
    | _ ->
      failwith "Arity of operator does not agree with number of params!"

  (*
  let leibniz_apply ~term_db:tdb ~location operator operands =
    (* check if operator is leibniz *)
    (* compute level *)
    let level = List.fold_left
        (fun m eo -> level_of_expr_or_op_arg eo |> max m)
        (level_of_operator tdb operator)
        operands
    in
    { location; level; operator; operands; }
  *)

  let const_app ~term_db:tdb ~location operator =
    let level = Op.level_of tdb operator in
    { location; level; operator; operands = [] }

  let const_unop builtin ~term_db:term_db ~location
      (op1:expr_or_op_arg) =
    let level = level_of_expr_or_op_arg op1 in
    let and_op = Builtin.get term_db builtin in
    let operator = FMOTA_op_def (O_builtin_op and_op) in
    apply ~term_db ~location ~level operator [op1]

  let const_binop builtin ~term_db:term_db ~location
      (op1:expr_or_op_arg) (op2:expr_or_op_arg) =
    let level = maxlevel
        (level_of_expr_or_op_arg op1)
        (level_of_expr_or_op_arg op2) in
    let and_op = Builtin.get term_db builtin in
    let operator = FMOTA_op_def (O_builtin_op and_op) in
    apply ~term_db ~location ~level operator [op1; op2]

  let neg       = const_unop Builtin.NOT
  let conj      = const_binop Builtin.AND
  let disj      = const_binop Builtin.OR
  let impl      = const_binop Builtin.IMPLIES
  let eqality   = const_binop Builtin.EQ
  let nequality = const_binop Builtin.NEQ

  let binop_fold conj neutral ~term_db ~location =
    let rec aux acc = function
      | [] ->
        Builtin.get term_db neutral |> OPD.bop |>
        Op.op_def |> const_app ~term_db ~location |> E.op_appl
      | [x] ->
        acc x
      | x :: xs ->
        aux (fun y -> conj ~term_db ~location (x |> EO.expr) (y |> EO.expr)
                      |> E.op_appl |> acc ) xs
    in
    aux (fun x -> x)

  let conjs = binop_fold conj Builtin.TRUE
  let disjs = binop_fold disj Builtin.FALSE
  let impls = binop_fold impl Builtin.TRUE

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
    match bounded_vars, q with
    | _ :: _, _ ->
      (* not all vars are bounded *)
      let msg = CCFormat.sprintf
          "Unbounded quantifier %a requires unbounded variables symbols %a"
          Builtin.pp q
          (CCFormat.list (fun f bs -> CCFormat.fprintf f "" ))
          bounded_vars
      in
      failwith msg
    | _, Builtin.EXISTS
    | _, Builtin.FORALL
    | _, Builtin.TEXISTS
    | _, Builtin.TFORALL ->
      { location; level;
        operator = FMOTA_op_def (O_builtin_op qop);
        operand;
        bound_symbols;
      }
    | _, _ ->
      let msg = CCFormat.sprintf "Operator %a is not an unbounded quantifier!" Builtin.pp q in
      failwith msg

  let bquantifier ~term_db:tdb ~location:location ~level:level q bound_symbols operand =
    let qop = Builtin.get tdb q in
    let unbounded_vars = List.filter
        (function
          | B_bounded_bound_symbol _ -> false;
          | B_unbounded_bound_symbol _ -> true;
        ) bound_symbols in
    match unbounded_vars, q with
    | _ :: _, _ ->
      let msg = CCFormat.sprintf
          "Bounded quantifier %a requires bounded variables symbols %a"
          Builtin.pp q
          (CCFormat.list (fun f bs -> CCFormat.fprintf f "" ))
          unbounded_vars
      in
      failwith msg
    | _, Builtin.BEXISTS
    | _, Builtin.BFORALL ->
      { location; level;
        operator = FMOTA_op_def (O_builtin_op qop);
        operand;
        bound_symbols;
      }
    | _, _ ->
      let msg = CCFormat.sprintf "Operator %a is not an unbounded quantifier!" Builtin.pp q in
      failwith msg

  let guards_of_binder ~term_db:tdb { location; level; operator;
                            operand; bound_symbols; } =
    let e_quants = List.map (Builtin.get tdb)
        [Builtin.EXISTS; Builtin.BEXISTS;
         Builtin.FORALL; Builtin.BFORALL;] in
    match operator with
    | FMOTA_op_def (O_builtin_op bop)
      when List.mem bop e_quants ->
      let b_in = Builtin.get tdb Builtin.SET_MEMBER
                 |> OPD.bop |> Op.op_def in
      let b_tuple = Builtin.get tdb Builtin.TUPLE
                 |> OPD.bop |> Op.op_def in
      let comp_level domain = List.fold_left
          (fun l fp ->
             let fpi = Deref.formal_param tdb fp in
             max l fpi.level
          ) (level_of_expr domain)
      in
      List.fold_left (fun aux ->
          function
          | B_bounded_bound_symbol {params; tuple = false; domain; } ->
            let location =  location_of_expr domain in
            let level = comp_level domain params in
            let guards = List.map (fun b ->
                apply ~term_db:tdb ~location ~level b_in
                  [ Op.formal_param b |> const_app ~term_db:tdb ~location
                    |> E.op_appl |> EO.expr ;
                    domain |> EO.expr
                  ]
              |> E.op_appl
              )
                params in
            guards
          | B_bounded_bound_symbol {params; tuple = true; domain; } ->
            let location =  location_of_expr domain in
            let level = comp_level domain params in
            let eo_params = List.map
                (fun fp ->
                   let op = Op.formal_param fp in
                   let level = Op.level_of tdb op in
                   apply ~term_db:tdb ~location ~level op []
                   |> E.op_appl |> EO.expr
                ) params
            in
            let tuple = apply ~term_db:tdb ~location ~level b_tuple eo_params in
            let guard = apply ~term_db:tdb ~location ~level b_in
                [tuple |> E.op_appl |> EO.expr; domain |> EO.expr ] in
            [guard |> E.op_appl ]
          | B_unbounded_bound_symbol ubs ->
            aux
        ) [] bound_symbols
    | _ ->
      let msg =
        CCFormat.sprintf "Can't extract guards of binder operator %a!"
          (fmt_operator tdb) operator
      in failwith msg
end
