open Kaputt.Abbreviations
open List
open Util

let positive = function
  (* 1 -- 2 -- 3
       \         /- 6
        \ 4 -- 5
                 \- 7
  *)
  | 1 -> [(1,2);(2,3);(1,4); (4,5); (5,6); (5,7)]
  (*      1 -- 2
          4 -- 3 -/
  *)
  | 2 -> [(1,2);(3,2);(4,3)]
  (*           1 -- 2
               5 -- 4 -- 3 -/
  *)
  | 3 -> [(5,4);(1,2);(3,2);(4,3)]
  | _ -> failwith "Error in test generation!"

let negative = function
  | 1 -> [(1,1)]
  | 2 -> [(1,2);(2,3);(3,1)]
  | 3 -> [(1,2);(2,3);(3,2)]
  | _ -> failwith "Error in test generation!"


let fmtOrdering lst = "[" ^ (mkString ~middle:"; " string_of_int lst) ^ "]"

let test_positive test_no =
  Test.make_simple_test
    ~title:("testing acyclic ordering " ^ (string_of_int test_no) ^
            " (expecting success)")
    (fun () ->
       let list = positive test_no in
       let ordering = find_ordering list in
       (* Printf.printf "obtained ordering: %s\n" (fmtOrdering ordering); *)
       let e = fold_left
           (fun errors pair ->
              let (x,y) = pair in
              match filter (fun v -> (v = x) || (v = y)) ordering with
              | [u;v] when u = x && v = y -> errors (* *)
              | other -> (pair, other) :: errors
           ) [] list
       in Assert.equal e []
    )

let test_negative test_no  =
  Test.make_simple_test
    ~title:("testing cyclic ordering " ^ (string_of_int test_no) ^
            " (expecting failure)")
    (fun () ->
       Assert.raises
         ~msg:"Expecting failure during ordering cyclic dependencies."
         (fun () -> find_ordering @$ negative test_no)
    )

let get_tests = append (map test_positive [1;2;3]) (map test_negative [1;2;3])
