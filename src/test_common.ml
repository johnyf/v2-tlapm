type test_result = {
    filename : string;
    mutable sany_context : Sany_ds.context option;
    mutable expr_context : Expr_ds.context option;
}

let mkTestResult ?sc ?ec name =
  {
    filename = name;
    sany_context = sc;
    expr_context = ec;
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
