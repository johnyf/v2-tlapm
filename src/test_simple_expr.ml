open Kaputt.Abbreviations
open Expr_ds
open Simple_expr_ds
open Expr_simple
open Util
open Test_common
open Format

let test_simple_expr record () =
  ignore (
      let rec obligations_to_string obligations n =
	match obligations with
	| [] -> ("","")
	| obl::tl ->
	   let (s1,s2) = obligations_to_string tl (n+1) in
	   let fft = str_formatter in
	   fprintf fft "Obligation %d:\n%a\n\n" n
		   Obligation_formatter.fmt_obligation obl;
	   let s1' = flush_str_formatter () in
	   fprintf fft "Obligation %d:\n%a\n\n" n
		     Simple_obligation_formatter.fmt_obligation (Simple_obligation.obligation_to_simple_obligation obl);
	   let s2' = flush_str_formatter () in
	   (s1'^s1,s2'^s2)
      in

      let (s1,s2) = obligations_to_string record.obligations 0 in
      Assert.equal ~msg:"Comparing expr and simple expr." s1 s2;
    )

let create_test record =
  Test.make_assert_test
    ~title: ("Comparing simple_expression to expression in " ^ record.filename)
    (fun () -> ())
    (fun () ->
      exhandler ( test_simple_expr record )
    )
    (fun () -> ()  )


let get_tests records = List.map create_test records
