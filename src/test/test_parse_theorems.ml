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

    method statement (flags, constrs) =
      let rec aux f = function
        | N_assume_prove {suffices = true; _} ->
          f+1
        | N_assume_prove _
        | N_expr _ ->
          f
        | N_ap_subst_in {body; _} ->
          (* should we also count the instantiated ones? in theory
             there should be no assume prove possible in there but who knows
          *)
          aux f body
      in
      function
      | ST_FORMULA f ->
        let flags = aux flags f in
        super#statement (flags, constrs) (ST_SUFFICES f)
      | ST_SUFFICES f ->
        let flags = aux flags f in
        super#statement (flags, constrs+1) (ST_SUFFICES f)
      | _ as st ->
        super#statement (flags, constrs) st
  end

let test_parse_theorems record () =
  Printf.printf "%s\n" record.filename;
  let context = match record.explicit_lambda_context with
    | Some x -> x
    | None -> failwith ("No expression content for " ^ record.filename ^ "!")
  in
  Printf.printf "Theorem correction conversion test of %s \n" record.filename;
  let mapped_context = expr_parse_theorems_context context in
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
