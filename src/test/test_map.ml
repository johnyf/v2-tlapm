open Kaputt.Abbreviations
open Any_expr
open Expr_ds
open Expr_map
open Expr_formatter
open Util
open Test_common


let test_expr_map record () =
  Printf.printf "%s\n" record.filename;
  let mapper = new expr_map in
  let context = match record.expr_context with
    | Some x -> x
    | None -> failwith ("No expression content for " ^ record.filename ^ "!")
  in
  let me = mapper#get_macc_extractor in
  let mapped_context =
    me#context (mapper#context (Any_context context, []) context) in
  (*  let result = context = mapped_context in *)
  (*  Printf.printf "%s\n%b\n" record.filename result;
      expr_formatter#context (Format.std_formatter, context, true, Module, 0)
                           context; *)
  Assert.equal ~msg:"Data-structures must be identical after id map!"
    context mapped_context


let test_map record =
  Test.make_assert_test
    ~title: ("mapping on expr ds trees " ^ record.filename)
    (fun () -> ())
    (test_expr_map record)
    (* (* Assert.equal throws an exception - this only works if there are no
          assertions inside *)
        (fun () ->
         Assert.no_raise ~msg:"Unexpected exception raised."
                         (fun () -> exhandler ( test_expr_map record )  )
        )
    *)
    (fun () -> ()  )


let get_tests records = List.map test_map records
