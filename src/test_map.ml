open Kaputt.Abbreviations
open Expr_ds
open Expr_map
open Util
open Test_common


let test_expr_map record () =
  Printf.printf "%s\n" record.filename;
  let mapper = new expr_map in
  let context = match record.expr_context with
  | Some x -> x
  | None -> failwith ("No expression content for " ^ record.filename ^ "!")
  in
  let mapped_context = mapper#context (Any_context context, []) in
  (* TODO: insert check on mapper *)
  ()


let test_map record =
  Test.make_assert_test
    ~title: ("mapping on expr ds trees " ^ record.filename)
    (fun () -> ())
    (fun () ->
     Assert.no_raise ~msg:"Unexpected exception raised."
                     (fun () -> exhandler ( test_expr_map record )  )
    )
    (fun () -> ()  )


let get_tests records = List.map test_map records
