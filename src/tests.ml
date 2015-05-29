open Printf
open Test_common
open Kaputt.Abbreviations

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


let addpath = (fun (str : string) -> "test/resources/" ^ str ^ ".xml")

let flt x = x

let files =
  List.map
  addpath (flt
           [
           "empty";
           "UserDefOp";
           "lambda";
           "tuples";
           "Choose";
           "at" ;
           "expr" ;
           "instanceA" ;
           "Euclid";
           (*           "exec"; *)
           "proofsteps";
           (* "priming_stephan"; *)
           "withsubmodule";
           "OneBit";
           (* contains duplicates of multiple modules, takes long to load *)
           (*"pharos";  *)
           ])

let () =
  let results = List.map (fun fn -> mkTestResult fn) files in
  let tests =
    List.concat [
        Test_util.get_tests;
        Test_sany.get_tests results;
        (*        Test_formatter.get_tests results; *)
      ] in
  match check_xmloutput with
  | true  ->
    printf "Creating XML output\n";
    let channel = open_out "test/results.xml" in
    Test.run_tests ~output:(Xml_junit_output channel)  tests;
  | false ->
    printf "Launching Tests\n";
    Test.run_tests tests
