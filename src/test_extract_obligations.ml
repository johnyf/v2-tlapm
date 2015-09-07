open Kaputt.Abbreviations
open Commons
open Expr_ds
open Extract_obligations
open Util
open Test_common
open Format
open Obligation

let fmt_prover = function
  | Isabelle -> "Isabelle"
  | Zenon -> "Zenon"
  | SMT -> "SMT"
  | LS4 -> "LS4"

let test_extract_obligations record () =
  ignore (
      let context = match record.explicit_lambda_context with
        | Some c -> c
        | None ->
           failwith ("Test implementation error! No expression context "^
                     "with corrected lamba expressions " ^
                     "available in record for file " ^ record.filename )
      in
      let cc = emptyCurrentContext context.entries in
      let eo = new extract_obligations in
      let (_,obs,_,_) =
        List.fold_left eo#mule (cc, [], Module, None) context.modules in
      match obs with
      | [] -> Printf.printf "%s no obligations extracted!\n" record.filename;
      | _  -> List.map (fun o ->
                let pstr = mkString fmt_prover (o.provers) in
                Printf.printf "%s obligation provers : %s\n" record.filename pstr
               ) obs;
      ()
    )

let create_test record =
  Test.make_assert_test
    ~title: ("extracting obligations " ^ record.filename)
    (fun () -> ())
    (fun () ->
     Assert.no_raise ~msg:"Unexpected exception raised."
                     (fun () -> exhandler ( test_extract_obligations record )  )
    )
    (fun () -> ()  )


let get_tests records = List.map create_test records
