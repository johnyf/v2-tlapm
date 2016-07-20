open Kaputt.Abbreviations
open Any_expr
open Expr_ds
open Expr_correct_lambda
open Expr_formatter
open Util
open Test_common


let test_expr_map record () =
  Printf.printf "%s\n" record.filename;
  let mapper = new correct_lambda in
  let context = match record.expr_context with
  | Some x -> x
  | None -> failwith ("No expression content for " ^ record.filename ^ "!")
  in
  let me = mapper#get_macc_extractor in
  let mapped_context =
    me#context (mapper#context (Nothing, []) context) in
  let result = context = mapped_context in
  record.explicit_lambda_context <- Some mapped_context;
  Printf.printf "Lambda conversion test of %s %b\n" record.filename result;
  (* (* don't print the lambda versions right now *)
  expr_formatter#context (Format.std_formatter, mapped_context, true, Module, 0)
                         mapped_context;  *)
  (*  Assert.equal context mapped_context *)
  ()


let test_map record =
  Test.make_assert_test
    ~title: ("correcting lambda on expr ds trees " ^ record.filename)
    (fun () -> ())
    (fun () ->
     Assert.no_raise ~msg:"Unexpected exception raised."
                     (fun () -> exhandler ( test_expr_map record )  )
    )
    (fun () -> ()  )


let get_tests records = List.map test_map records
