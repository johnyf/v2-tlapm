open Kaputt.Abbreviations
open Commons
open Expr_ds
open Extract_obligations
open Util
open Test_common
open Format
open Obligation
open Obligation_formatter


let test_extract_obligations record () =
  let context = match record.explicit_steps_context with
    | Some c -> c
    | None ->
      failwith ("Test implementation error! No expression context "^
                "with corrected proof steps " ^
                "available in record for file " ^ record.filename )
  in
  let cc = emptyCurrentContext context.entries in
  let eo = new extract_obligations in
  let acc = EOAcc ([cc], [], Module, context.root_module, None) in
  let acc1 = eo#context acc context in
  let obs = get_obligations acc1 in
  begin
    match obs with
    | [] -> Printf.printf "%s no obligations extracted!\n" record.filename;
    | _  ->
      begin
        let total = List.length obs in
        let fmt_ps = fmt_list fmt_prover in
        let print_obl i o =
          Format.printf "Obligation #%d of %d@." i total;
          Format.printf "%s obligation provers : %a@." record.filename fmt_ps o.provers;
          (* Printf.printf "%d assumptions\n" (List.length o.goal.assumes); *)
          fmt_obligation std_formatter o;
          Format.printf "@.(end of obligation)@.";
          i+1
        in
        ignore (
          Format.printf "%s : no of obligations: %d@." record.filename total;
          List.fold_left print_obl 1 obs
        )
      end
  end;
  obs

let create_test record =
  Test.make_assert_test
    ~title: ("extracting obligations " ^ record.filename)
    (fun () -> ())
    ( test_extract_obligations record )
    (fun obs -> record.obligations <- obs; ()   )


let get_tests records = List.map create_test records
