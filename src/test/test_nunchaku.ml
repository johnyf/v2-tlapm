open Kaputt
open Util
open Test
open Test_common
open Expr_ds
open Tla_simple_pb
open List
open Nunchaku
open Expr_formatter
open Tla_mod
open Obligation

let re_positive = Str.regexp ".*nunchaku.*positive"
let re_negative = Str.regexp ".*nunchaku.*negative"

let settings = { Settings.default_settings with
                 Settings.nunchaku_temp_path =
                   autodetect_executable_path ^ "/../../nunchaku"
               }

let make_test_positive record =
  let test_fun obl =
    let msg str = Format.asprintf
        "Expecting a counter-model to %a, but nunchaku returned %s"
        (fmt_assume_prove obl.term_db) obl.goal str
    in
    match nunchaku settings obl obl.id with
    | REFUTED model ->
      Format.printf "Found a counter model for obligation %a (as expected).@."
        (fmt_assume_prove obl.term_db) obl.goal;
      ()
    | VALID ->
      Assertion.fail_msg (msg "valid")
    | UNKNOWN ->
      Assertion.fail_msg (msg "unknown!")
    | TIMEOUT ->
      Assertion.fail_msg (msg "timed out!")
  in
  let title = Format.asprintf
      "Testing: file %s: nunchaku finds a model for all obligations"
      record.filename
  in
  make_simple_test ~title (fun () ->
      ignore( map test_fun record.obligations )
    )

let make_test_negative record =
  let test_fun obl =
    let msg = Format.asprintf
        "Expecting no counter-model to %a, but nunchaku returned one!"
        (fmt_assume_prove obl.term_db) obl.goal
    in
    match nunchaku settings obl obl.id with
    | REFUTED model ->
      Assertion.fail_msg msg
    | VALID | UNKNOWN | TIMEOUT ->
      Format.printf "No counter-model for obligation %a (as expected).@."
        (fmt_assume_prove obl.term_db) obl.goal
      ;
      ()
  in
  let title = Format.asprintf
      "Testing %s: nunchaku finds no model for valid obligations."
      record.filename
  in
  make_simple_test ~title (fun () ->
      ignore( map test_fun record.obligations )
    )


let get_tests records =
  let positive_tests =
    filter (fun r -> Str.string_match re_positive r.filename 0) records
  in
  let negative_tests =
    filter (fun r -> Str.string_match re_negative r.filename 0) records
  in
  if (length positive_tests) <= 0 then
    failwith "No positive nunchaku tests in records!";
  if (length negative_tests) <= 0 then
    failwith "No negative nunchaku tests in records!";
  let ptests = map make_test_positive positive_tests  in
  let ntests = map make_test_negative negative_tests in
  if (length ptests) <= 0 then
    failwith "No positive nunchaku test cases generated!";
  if (length ntests) <= 0 then
    failwith "No negative nunchaku test cases generated!";
  append ptests ntests
