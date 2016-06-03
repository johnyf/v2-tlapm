open Printf
open Test_common
open Kaputt.Abbreviations
open Kaputt.Test

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


let addpath = (fun (str : string) -> "test/resources/xml/" ^ str ^ ".xml")

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
           "obligation_bug1" (* TODO: fix this bug! *)
           ])

let () =
  let results = List.map (fun fn -> mkTestResult fn) (files id) in
  (* In the XML format, the expression ASSUME I!T PROVE X uses a theorem
     reference for I!T, but in reality is in an AP substitution. Remove the
     test case till this is fixed.
  *)
  let fmt_filter r = r.filename <> (addpath "withsubmodule")
  in
  let without_broken = List.filter fmt_filter results in
  let tests =
    List.concat [
        Test_util.get_tests;
        Test_sany.get_tests results;
        Test_map.get_tests results;
        Test_correct_lambda.get_tests without_broken;
        Test_parse_theorems.get_tests without_broken;
        Test_extract_obligations.get_tests without_broken;
        Test_formatter.get_tests without_broken (* *);
        Test_issue2.get_tests without_broken;
      ] in
  match check_xmloutput with
  | true  ->
    printf "Creating XML output\n";
    let channel = open_out "test/results.xml" in
    Test.run_tests ~output:(Xml_junit_output channel)  tests;
  | false ->
    printf "Launching Tests\n";
    Test.run_tests tests
