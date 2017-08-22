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
  let old_context = match record.expr_context with
    | Some x -> x
    | None -> failwith ("No expression content for " ^ record.filename ^ "!")
  in
  let me = mapper#get_macc_extractor in
  let acc = mapper#context
      (Nothing, (old_context.entries, IntMap.empty))
      old_context in
  let new_context = me#context acc  in
  let mids = Expr_termdb_utils.mentioned_ids new_context.entries  in
  let eids = Expr_termdb_utils.entry_ids new_context.entries  in
  let incons = Expr_termdb_utils.inconsistent_entries new_context.entries in
  let msg = CCFormat.sprintf
      "Mids: %a;\nEids: %a;\nConflicts: %a;"
      (CCFormat.list CCFormat.int) mids
      (CCFormat.list CCFormat.int) eids
      (CCFormat.list CCFormat.int) incons
  in
  Printf.printf "%s\n" msg;
  Printf.printf "Lambda conversion test of %s \n" record.filename;
  Assert.equal
    ~prn:(CCFormat.sprintf "[%a]" (CCFormat.list ~sep:(fun x () ->
        CCFormat.string x "; ") CCFormat.int)
      )
    ~msg:"Term db after lambda conversion is consistent"
    (Expr_termdb_utils.inconsistent_entries new_context.entries)
    []
  ;
  record.explicit_lambda_context <- Some new_context;
  ()


let test_map record =
  Test.make_assert_test
    ~title: ("correcting lambda on expr ds trees " ^ record.filename)
    (fun () -> ())
    (fun () ->
       Assert.no_raise ~msg:"Unexpected exception raised."
         (fun () -> exhandler ~fn:(Some record.filename) ( test_expr_map record )  )
    )
    (fun () -> ()  )


let get_tests records = List.map test_map records
