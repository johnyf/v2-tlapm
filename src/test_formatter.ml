open Kaputt.Abbreviations
open Expr_ds
open Expr_formatter
open Util
open Test_common
open Format


let test_sany record () =
  ignore (
      let context = match record.expr_context with
        | Some c -> c
        | None ->
           failwith ("Test implementation error! No expression context "^
                       "available in record for file " ^ record.filename )
      in
      (* let fmt = new formatter in *)
      (*      pp_set_margin std_formatter 80; *)
      fprintf std_formatter "@[<v 0>%s:@\n" record.filename;
      let fmt_string =
        expr_formatter#context (std_formatter, context, true, Module) context in
      fprintf std_formatter "@]@.";
      ()
    )

let create_test record =
  Test.make_assert_test
    ~title: ("formatting " ^ record.filename)
    (fun () -> ())
    (fun () ->
     Assert.no_raise ~msg:"Unexpected exception raised."
                     (fun () -> exhandler ( test_sany record )  )
    )
    (fun () -> ()  )


let get_tests records = List.map create_test records
