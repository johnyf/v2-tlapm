open Commons
open Util
open Expr_ds

module Builtin = struct
  exception BuiltinNotFound of string * string

  type builtin_symbol =
    (* logical operators *)
    | TRUE
    | FALSE
    | EQ
    | NEQ
    | NOT
    | AND
    | OR
    (* quantifiers *)
    | IMPLIES
    | FORALL
    | EXISTS
    | BFORALL
    | BEXISTS
    (* temporal operators *)
    | PRIME
    | TFORALL
    | TEXISTS
    | BOX
    | DIAMOND
    | SQ_BRACK
    | ANG_BRACK
    | WF
    | SF
    (* tuples, functions, records *)
    | TUPLE
    | FUN_APP
    | FUN_CONSTR
    | RCD_CONSTR
    | SET_ENUM
    | IF_THEN_ELSE

  let builtin_location = {
    line     = mkDummyRange;
    column   = mkDummyRange;
    filename = "--TLA+ BUILTINS--";
  }

  let new_id tdb =
    List.fold_left (fun m (curr,_) -> if curr > m then curr else m) 0 tdb
    |> (+) 1

  let formal_param tdb arity i =
    let id = new_id tdb in
    let tdb0 = List.append tdb [
        (id, FP_entry { id;
                        location = builtin_location;
                        level = None;
                        name = "fparam" ^ (string_of_int i);
                        arity;
                      })]
    in
    (tdb0, FP_ref id)



  let builtin id level name arity params =
    (BOP_ref id, {id; level=Some level;name;arity;params})

  module Make = struct
    (* functions for creating new builtins in a term database -- not exposed *)
    let mk_builtin tdb level name arity params =
      (* create fresh formal parameters *)
      let tdb1, rparams, _ =
        List.fold_left (fun (tdb,ps,i) (ar,leibniz) ->
            let tdb0, fp = formal_param tdb ar i in
            (tdb0, (fp,leibniz)::ps,i+1)
          ) (tdb,[],0) params
      in
      let params = List.rev rparams in
      (* create the builtin and add it to the term db *)
      let id = new_id tdb1 in
      let bi_ref, bi = builtin id level name arity params in
      let tdb2 = List.append tdb1 [(id, BOP_entry bi)] in
      (tdb2, bi_ref)

    let builtin_true tdb = mk_builtin tdb ConstantLevel "TRUE" 0 []

    let builtin_false tdb = mk_builtin tdb ConstantLevel "FALSE" 0 []

    (* TODO: check if this is correct - in sany the quantifier has arity -1 *)
    let bounded_exists tdb =
      mk_builtin tdb ConstantLevel "$BoundedExists" 1 [(0,true)]

    let unbounded_exists tdb =
      mk_builtin tdb ConstantLevel "$UnboundedExists" 1 [(0,true)]

    let bounded_forall tdb =
      mk_builtin tdb ConstantLevel "$BoundedForall" 1 [(0,true)]

    let unbounded_forall tdb =
      mk_builtin tdb ConstantLevel "$UnboundedForall" 1 [(0,true)]

    let set_in tdb =
      mk_builtin tdb ConstantLevel "\\in" 2 [(0,true); (0,true)]

    let tuple tdb =
      mk_builtin tdb ConstantLevel "$Tuple" (-1) []

    let if_then_else tdb =
      mk_builtin tdb ConstantLevel "$IfThenElse" 3 [(0,true);(0,true);(0,true)]
  end

  let string_of_builtin = function
    | TRUE -> "TRUE"
    | FALSE -> "FALSE"
    | NOT -> "\\lnot"
    | AND -> "\\land"
    | OR -> "\\lor"
    | IMPLIES -> "=>"
    | FORALL -> "$UnboundedForall"
    | EXISTS -> "$UnboundedExists"
    | BFORALL -> "$BoundedForall"
    | BEXISTS  -> "$BoundedExists"
    | PRIME -> "'"
    | TFORALL -> "$TemporalForall"
    | TEXISTS -> "$TemporalExists"
    | EQ -> "="
    | NEQ -> "/="
    | BOX -> "[]"
    | DIAMOND -> "<>"
    | WF -> "$WF"
    | SF -> "$SF"
    | FUN_APP -> ""
    | SET_ENUM -> "$SetEnumerate"
    | SQ_BRACK -> "$SquareAct"
    | ANG_BRACK -> "$AngleAct"
    | IF_THEN_ELSE -> "$IfThenElse"
    | TUPLE -> "$Tuple"
    | RCD_CONSTR -> "$RcdConstructor"
    | FUN_CONSTR -> "$FcnConstructor"

  let builtin_of_string = function
    | "TRUE" -> TRUE
    | "FALSE" -> FALSE
    | "\\lnot" -> NOT
    | "\\land" -> AND
    | "\\lor" -> OR
    | "=>" -> IMPLIES
    | "$UnboundedForall" -> FORALL
    | "$UnboundedExists" -> EXISTS
    | "$BoundedForall" -> BFORALL
    | "$BoundedExists" -> BEXISTS
    | "'" ->              PRIME
    | "$TemporalForall" ->TFORALL
    | "$TemporalExists" ->TEXISTS
    | "=" ->              EQ
    | "/=" ->             NEQ
    | "[]" ->             BOX
    | "<>" ->             DIAMOND
    | "$WF" ->            WF
    | "$SF" ->            SF
    | "$FcnApply" ->      FUN_APP
    | "$FcnConstructor" -> FUN_CONSTR
    | "$RcdConstructor" -> RCD_CONSTR
    | "$Tuple"          -> TUPLE
    | "$SetEnumerate" ->  SET_ENUM
    | "$SquareAct" ->     SQ_BRACK
    | "$AngleAct" ->      ANG_BRACK
    | "$IfThenElse" ->    IF_THEN_ELSE
    | str -> let msg = CCFormat.sprintf "Unknown builtin %s" str in
      failwith msg

  (** get an entry *)
  let get (tdb:term_db) b =
    let lookup x =
      match List.fold_left
              (fun acc (id, e) ->
                 match e with
                 | BOP_entry {name;_} when name = x ->
                   Some (BOP_ref id)
                 | _ ->
                   acc
              ) None tdb
      with
      | Some entry -> entry
      | None ->
        let msg = CCFormat.sprintf "Builtin %s not found in term db!" x in
        raise (BuiltinNotFound (msg, x))
    in
    lookup (string_of_builtin b)

  let create_entry (tdb:term_db) s f =
    try
      (* get the builtin from the term db *)
      (tdb, get tdb s)
    with
    | BuiltinNotFound (_,_) ->
      (* if it fails, create a new term from f *)
      f tdb

  (** fetches a builtin and creates it, if it doesn't exist yet *)
  let fetch tdb s =
    let ce = create_entry tdb s in
    match s with
    | TRUE -> ce Make.builtin_true
    | FALSE -> ce Make.builtin_false
    | FORALL -> ce Make.unbounded_forall
    | EXISTS -> ce Make.unbounded_exists
    | BFORALL -> ce Make.bounded_forall
    | BEXISTS -> ce Make.bounded_exists
    | _ -> failwith "builtin symbol not yet supported"

  let complete_builtins tdb =
    (* TODO: add builtins for all the operators here *)
    (*
    let ops = [ NOT; AND; OR; IMPLIES; FORALL; EXISTS; BFORALL; BEXISTS;
                TFORALL; TEXISTS; TBFORALL; TBEXISTS; BOX; DIAMOND; WF; SF;
                FUNAPP; SET_ENUM ] in
    *)
    let ops = [TRUE; FALSE; FORALL; EXISTS; BFORALL; BEXISTS ] in
    List.fold_left (fun db op -> fetch db op |> fst) tdb ops
end
