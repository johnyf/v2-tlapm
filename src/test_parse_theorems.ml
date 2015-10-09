open Kaputt.Abbreviations
open Any_expr
open Expr_ds
open Expr_parse_theorems
open Expr_formatter
open Util
open Test_common
open Expr_visitor


class suffices_scanner =
object
inherit [int * int] visitor as super

method statement (flags, constrs) = function
  | ST_FORMULA f ->
     let flags = if f.suffices then flags+1 else flags in
     super#statement (flags, constrs) (ST_FORMULA f)
  | ST_SUFFICES f ->
     let flags = if f.suffices then flags+1 else flags in
     super#statement (flags, constrs+1) (ST_SUFFICES f)
  | _ as st ->
     super#statement (flags, constrs) st
end

let test_parse_theorems record () =
  Printf.printf "%s\n" record.filename;
  let mapper = new expr_parse_theorems in
  let context = match record.explicit_lambda_context with
  | Some x -> x
  | None -> failwith ("No expression content for " ^ record.filename ^ "!")
  in
  Printf.printf "Theorem correction conversion test of %s \n" record.filename;
  let me = mapper#get_macc_extractor in
  let init_acc = (Any_context context, (Some context.entries,[])) in
  let macc = mapper#context init_acc context in
  let mapped_context = me#context macc in
  let scanner = new suffices_scanner in
  let (a, b) = List.fold_left scanner#mule (0,0) context.modules in
  let (c, d) = List.fold_left scanner#mule (0,0) mapped_context.modules in
  Printf.printf "constr: %d %d -- %d %d\n" a c b d;
  record.explicit_steps_context <- Some mapped_context;
  ()

let test_map record =
  Test.make_assert_test
    ~title: ("making proof steps explicit " ^ record.filename)
    (fun () -> ())
    (fun () ->
     Assert.no_raise ~msg:"Unexpected exception raised."
                     (fun () -> exhandler ( test_parse_theorems record )  )
    )
    (fun () -> ()  )


let get_tests records = List.map test_map records
