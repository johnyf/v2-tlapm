open Kaputt.Abbreviations
open Expr_ds
open Expr_formatter
open Util
open Test_common
open Format

let test_sany record () =
  ignore (
    let err = CCFormat.sprintf
        "Test implementation error! Record faulty in file %s." record.filename
    in
    let context = CCOpt.get_exn record.explicit_steps_context in
    (* let fmt = new formatter in *)
    (*      pp_set_margin std_formatter 80; *)
    fprintf str_formatter "@[<v 0>%s:@\n" record.filename;
    let init = (str_formatter, context.entries, true, Module, 0) in
    ignore ( expr_formatter#context init context );
    fprintf str_formatter "@]@.";
    let text = flush_str_formatter () in
    if (String.length text) < 1000 then
      (* only print to screen if it is not too big*)
      Format.printf "%s@." text
    else
      Format.printf
        "skipping too large pretty printing of test %s@."
        record.filename;
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
