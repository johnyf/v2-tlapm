open Kaputt.Abbreviations
open Expr_ds
open Simple_expr_ds
open Simple_expr
open Util
open Test_common
open Format
open Tla_simple_pb
open Backend_exceptions
open Obligation_formatter

let test_simple_expr record () =
  let rec obligations_to_string obligations n =
    match obligations with
    | [] -> ("","")
    | obl::tl ->
      let (s1,s2) = obligations_to_string tl (n+1) in
      let fft = str_formatter in
      fprintf fft "Obligation %d:\n%a\n\n" n
        fmt_obligation obl;
      let s1' = flush_str_formatter () in
      fprintf fft "Obligation %d:\n%a\n\n" n
        fmt_tla_simple_pb (tla_pb_to_tla_simple_pb obl);
      let s2' = flush_str_formatter () in
      (s1'^s1,s2'^s2)
  in
  try
    let (s1,s2) = obligations_to_string record.obligations 0 in
    let msg = sprintf "Comparing expr %s and simple expr %s.@." s1 s2 in
    (* note: some language elements are removed, e.g. theorem references in a
       term are replaced by the definition. Therefore this assertion doesn't
       hold in general. *)
    Assert.equal ~msg s1 s2;
    ()
  with
  | UnhandledLanguageElement (_, _)-> ()

let create_test record =
  Test.make_assert_test
    ~title: ("Comparing simple_expression to expression in " ^ record.filename)
    (fun () -> ())
    (fun () ->
       ( test_simple_expr record () )
    )
    (fun () -> ()  )


let get_tests records = List.map create_test records
