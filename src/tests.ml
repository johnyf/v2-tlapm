open Printf
open Test_common
open Kaputt.Abbreviations
open Kaputt.Test
open List

let t1 =
  Test.make_simple_test
    ~title:"first test"
    (fun () -> Assert.equal_int 3 3)

(* checks if the environment variable TLAPM_TEST_OUTPUT is set to xml *)
let check_xmloutput =
  try
    let value = Sys.getenv "TLAPM_TEST_OUTPUT" in
    value = "xml"
  with Not_found -> false


let addpath = (fun (str : string) -> "test/xml/" ^ str ^ ".xml")

let id x = true

let files flt =
  List.map
    addpath (List.filter flt
               [
                 "empty";
                 "UserDefOp";
                 "lambda";
                 "minilambda";
                 "tuples";
                 "Choose";
                 "at" ;
                 "expr" ;
                 "functions";
                 "instanceA" ;
                 "Euclid";
                 "Quantifiers";
                 "case_take_use_hide";
                 "qed";
                 "qed2";
                 "extends_use_def_outer";
                 (*           "exec"; *)
                 "proofsteps";
                 "priming_stephan";
                 (* "withsubmodule"; has incorrect use of theorem *)
                 "OneBit";
                 (* contains duplicates of multiple modules, takes long to load *)
                 (*"pharos";  *)
                 "obligation_bug1"; (* TODO: fix this bug! *)
                 "bug02";
                 "nunchaku";
                 "nunchaku_tests_positive";
                 "nunchaku_tests_negative";
               ])

let () =
  let results = List.map mkTestResult (files id) in
  (* In the XML format, the expression ASSUME I!T PROVE X uses a theorem
     reference for I!T, but in reality is in an AP substitution. Remove the
     test case till this is fixed.
  *)
  let fmt_filter r = r.filename <> (addpath "withsubmodule") in
  let fmt_nun_filter r =
    not (mem r.filename
           (map addpath  ["withsubmodule";
                          (* this test has an obligation <1>1 => a=b which
                             translates (correctly) to a=b => a=b *)
                          "case_take_use_hide"
                         ]))
  in
  let without_broken = List.filter fmt_filter results in
  let without_nunchaku_broken = List.filter fmt_nun_filter without_broken in
  let tests =
    List.concat [
      Test_util.get_tests;
      Test_sany.get_tests results;
      (*
      Test_map.get_tests results;
      Test_correct_lambda.get_tests without_broken;
      Test_parse_theorems.get_tests without_broken;
      Test_extract_obligations.get_tests without_broken;
      Test_formatter.get_tests without_broken (* *);
      Test_substitution.get_tests;
      Test_simple_expr.get_tests without_nunchaku_broken;
      Test_nunchaku.get_tests without_nunchaku_broken;
      Test_issue2.get_tests without_broken;
      *)
    ] in
  match check_xmloutput with
  | true  ->
    printf "Creating XML output\n";
    let channel = open_out "test/results.xml" in
    Test.run_tests ~output:(Xml_junit_output channel)  tests;
  | false ->
    printf "Launching Tests\n";
    Test.run_tests tests
