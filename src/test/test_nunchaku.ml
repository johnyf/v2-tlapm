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
open Backend_exceptions

let re_positive = Str.regexp ".*nunchaku.*positive"
let re_negative = Str.regexp ".*nunchaku.*negative"

let settings = { Settings.default_settings with
                 Settings.nunchaku_temp_path =
                   autodetect_executable_path ^ "/../../nunchaku"
               }

type nun_result =
  | Ok of obligation
  | Fail of obligation * string
  | Unhandled of obligation


let test_fun_positive obl =
  try
    let msg str = Format.asprintf
        "Expecting a counter-model to %a, but nunchaku returned %s"
        (fmt_assume_prove obl.term_db) obl.goal str
    in
    match nunchaku settings obl obl.id with
    | REFUTED model ->
      Format.printf "Found a counter model for obligation %a (as expected).@."
        (fmt_assume_prove obl.term_db) obl.goal;
      Ok obl
    | VALID
    | UNKNOWN
    | TIMEOUT ->
      Fail (obl, msg "valid/unknown/timout")
  with
  | UnhandledLanguageElement (p, msg) ->
    Format.printf "Warning: skipped obligation %d because of unhandled \
                   element %s@." obl.id msg;
    Unhandled obl


let test_fun_negative obl =
  try
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
      Ok obl
  with
  | UnhandledLanguageElement (_, msg) ->
    Format.printf "Warning: skipped obligation %d because of unhandled \
                   element %s@." obl.id msg;
    Unhandled obl

let title_positive record = Format.asprintf
    "Testing: file %s: nunchaku finds a model for all obligations"
    record.filename

let title_negative record = Format.asprintf
      "Testing %s: nunchaku finds no model for valid obligations."
      record.filename


let make_nun_test test_fun p_title record =
  let title = p_title record in
  make_simple_test ~title (fun () ->
      let results = map test_fun record.obligations in
      let failed = filter (function | Fail _ -> true | _ -> false) results in
      let failed_str = Format.asprintf "%a"
          (fmt_list
             (fun f -> function
                | Fail ({id; _}, msg) -> Format.fprintf f "%d : %s" id msg
                | _ -> failwith "Implementation error in test."
             )
          ) failed in
      Assertion.equal ~msg:"list of failed obligations must be empty."
        failed_str "[]"
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
  let ptests = map (make_nun_test test_fun_positive title_positive)
      positive_tests  in
  let ntests = map (make_nun_test test_fun_negative title_negative)
      negative_tests in
  if (length ptests) <= 0 then
    failwith "No positive nunchaku test cases generated!";
  if (length ntests) <= 0 then
    failwith "No negative nunchaku test cases generated!";
  append ptests ntests
