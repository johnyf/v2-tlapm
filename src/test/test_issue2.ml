open Kaputt
open Kaputt.Abbreviations
open Any_expr
open Expr_ds
open Expr_map
open Expr_formatter
open Obligation
open Obligation_formatter
open Format
open Util
open Test_common



let test_map record =
  Test.make_assert_test
    ~title: ("testing bug of issue 2:" ^ record.filename)
    (fun () -> record )
    (function
        { obligations; _ } ->
        Assert.is_false (obligations = []);
        let _ = List.map (function
            | { goal = N_expr _; _ }
            | { goal = (N_assume_prove { assumes = []; _}); _ } ->
              ()
            | obl ->
              let msg = asprintf
                  "Expected no assumptions in goal of obligation %a"
                  fmt_obligation obl
              in
              Assert.fail_msg msg
          ) obligations;
        in
        ()
    )
    (fun () -> ()  )


let get_tests records =
  let re = Str.regexp ".*obligation_bug1.xml$" in
  let record = List.find (fun x -> Str.string_match re x.filename 0) records in
  [test_map record]
