open Commons
open Expr_ds

(* TODO: refactor *)

let builtin_location = {
  line     = mkDummyRange;
  column   = mkDummyRange;
  filename = "--TLA+ BUILTINS--";
}

let builtin_true = {
  id = -1;
  level = Some ConstantLevel;
  name = "TRUE";
  arity =0;
  params = []
}

let builtin_false = {
  id = -2;
  level = Some ConstantLevel;
  name = "FALSE";
  arity =0;
  params = []
}

(*
let formal_param i arity =
  {
    id = -3;
    location = builtin_location;
    level = None;
    name = "fparam" ^ (string_of_int i);
    arity;
  }
*)

let formal_param _ _ =
  failwith "Builtins must be refactored !"

let bounded_exists =
  {
    id = -4;
    name = "$BoundedExists";
    level = None;
    arity = 1;
    params = [(formal_param 0 0, false)];  (* TODO: check if this is correct - in sany the quantifier has arity -1 *)
  }

let unbounded_exists =
  {
    id = -5;
    name = "$UnboundedExists";
    level = None;
    arity = 1;
    params = [(formal_param 0 0, false)]; (* TODO: check if this is correct *)
  }

let bounded_forall =
  {
    id = -6;
    name = "$BoundedForall";
    level = None;
    arity = 1;
    params = [(formal_param 0 0, false)];  (* TODO: check if this is correct - in sany the quantifier has arity -1 *)
  }

let unbounded_forall =
  {
    id = -7;
    name = "$UnboundedForall";
    level = None;
    arity = 1;
    params = [(formal_param 0 0, false)]; (* TODO: check if this is correct *)
  }


let set_in =
  {
    id = -8;
    name = "\\in";
    level = None;
    arity = 2;
    params = [(formal_param 0 0, true)]
  }

let tuple =
  {
    id = -9;
    name = "$Tuple";
    level = None;
    arity = -1;
    params = [];
  }

let if_then_else =
  {
    id = -10;
    name = "$IfThenElse";
    level = None;
    arity = 3;
    params = [
      (formal_param 0 0, true);
      (formal_param 1 0, true);
      (formal_param 2 0, true);
    ];
  }
