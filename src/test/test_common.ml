open Kaputt

type test_result = {
  filename : string;
  mutable sany_context : Sany_ds.context option;
  mutable expr_context : Expr_ds.context option;
  mutable explicit_lambda_context : Expr_ds.context option;
  mutable explicit_steps_context : Expr_ds.context option;
  mutable obligations  : Obligation.obligation list;
  mutable simple_obligations : Simple_expr_ds.tla_simple_pb list;
}

let mkTestResult ?sc ?ec ?lc ?esc ?ob:(ob=[]) ?sob:(sob=[]) name =
  {
    filename = name;
    sany_context = sc;
    expr_context = ec;
    explicit_lambda_context = lc;
    explicit_steps_context = esc;
    obligations  = ob;
    simple_obligations = sob;
  }

let exhandler ?fn:(fn=None) f =
  try
    Printexc.record_backtrace true;
    let ret = f () in
    Printexc.record_backtrace false;
    ret
  with
  | Assertion.Failed( {Assertion.expected_value; actual_value; message;} ) as x ->
    CCOpt.iter (fun x -> Printf.eprintf "File: %s\n" x) fn;
    let bt = Printexc.get_backtrace () in
    Printf.eprintf "Assertion Failed: %s\n" (Printexc.to_string x);
    Printf.eprintf "Expected:%s\nGot     :%s\nmessage:%s\n"
      expected_value actual_value message;
    Printf.eprintf "Backtrace: %s\n\n" bt;
    raise x
  | x ->
    CCOpt.iter (fun x -> Printf.eprintf "File: %s\n" x) fn;
    let bt = Printexc.get_backtrace () in
    Printf.eprintf "Exception: %s\n" (Printexc.to_string x);
    Printf.eprintf "Backtrace: %s\n\n" bt;
    raise x
