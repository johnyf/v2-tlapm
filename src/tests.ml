open Printf

open Kaputt.Abbreviations

let t1 =
  Test.make_simple_test
    ~title:"first test"
    (fun () -> Assert.equal_int 3 3)
  
let () =
  printf "Launching Tests\n";
  Test.run_tests ( Test_sany.get_tests ) 


