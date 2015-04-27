open Printf

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

let () =
  let tests = List.concat [Test_sany.get_tests; Test_util.get_tests] in
  match check_xmloutput with
  | true  ->
    printf "Creating XML output\n";
    let channel = open_out "test/results.xml" in
    Test.run_tests ~output:(Xml_junit_output channel)  tests;
  | false ->
    printf "Launching Tests\n";
    Test.run_tests tests
