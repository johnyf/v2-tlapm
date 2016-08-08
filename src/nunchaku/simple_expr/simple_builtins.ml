open Commons
open Simple_expr_ds

let builtin_location = {
  line     = mkDummyRange;
  column   = mkDummyRange;
  filename = "--TLA+ BUILTINS--";
}

let builtin_true = { level = Some ConstantLevel;
                     name = "TRUE";
                     arity =0;
                     params = []
                   }

let builtin_false = { level = Some ConstantLevel;
                      name = "FALSE";
                      arity =0;
                      params = []
                    }

let formal_param i arity =
  FP {
    location = builtin_location;
    level = None;
    name = "fparam" ^ (string_of_int i);
    arity;
  }

let bounded_exists =
  { name = "$BoundedExists";
    level = None;
    arity = 1;
    params = [(formal_param 0 0, false)];  (* TODO: check if this is correct - in sany the quantifier has arity -1 *)
  }

let unbounded_exists =
  { name = "$UnboundedExists";
    level = None;
    arity = 1;
    params = [(formal_param 0 0, false)]; (* TODO: check if this is correct *)
  }

let set_in =
  { name = "\\in";
    level = None;
    arity = 2;
    params = [(formal_param 0 0, true)]
  }

let tuple =
  { name = "$Tuple";
    level = None;
    arity = -1;
    params = [];
  }

let if_then_else =
  { name = "$IfThenElse";
    level = None;
    arity = 3;
    params = [
      (formal_param 0 0, true);
      (formal_param 1 0, true);
      (formal_param 2 0, true);
    ];
  }
