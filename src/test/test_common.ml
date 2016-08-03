type test_result = {
  filename : string;
  mutable sany_context : Sany_ds.context option;
  mutable expr_context : Expr_ds.context option;
  mutable explicit_lambda_context : Expr_ds.context option;
  mutable explicit_steps_context : Expr_ds.context option;
  mutable obligations  : Obligation.obligation list;
}

let mkTestResult ?sc ?ec ?lc ?esc ?ob:(ob=[]) name =
  {
    filename = name;
    sany_context = sc;
    expr_context = ec;
    explicit_lambda_context = lc;
    explicit_steps_context = esc;
    obligations  = ob;
  }

let exhandler f =
  try
    Printexc.record_backtrace true;
    let ret = f () in
    Printexc.record_backtrace false;
    ret
  with
    x ->
    Printf.printf "Exception: %s\n" (Printexc.to_string x);
    Printf.printf "Backtrace: %s\n\n" (Printexc.get_backtrace ());
    raise x
