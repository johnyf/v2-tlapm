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
      let fmt = new formatter in
      let fmt_string = fmt#context (std_formatter, context) in
      ()
    )

let test_formatter record =
  Test.make_assert_test
    ~title: ("formatting " ^ record.filename)
    (fun () -> ())
    (fun () ->
     Assert.no_raise ~msg:"Unexpected exception raised."
                     (fun () -> exhandler ( test_sany record )  )
    )
    (fun () -> ()  )


let get_tests records = List.map test_formatter records
