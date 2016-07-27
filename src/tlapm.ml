open Commons
open Util
open Sany
open Obligation
open Extract_obligations
open Format
open Nunchaku
open Expr_substitution
open Settings
open Arg_handler
open Toolbox
open Result
open Backend_exceptions


(** Creates the command line string used to invoke the sany parser *)
let java_cmd { check_schema; java_path; include_paths; input_file; pm_path; _ } =
  let fmt_path = fmt_option ~none:"java" ~some:"" ~some_back:"" fmt_string in
  let fmt_include = fmt_list ~front:"" ~middle:""
                             ~back:""
                             (fun fmt s -> fprintf fmt "-I \"%s\" " s) in
  let fmt_offline = if check_schema then "" else "-o" in
  let cmd = asprintf "%a -jar %s/lib/sany.jar %s %a \"%s\""
                     fmt_path java_path
                     pm_path
                     fmt_offline
                     fmt_include include_paths
                     input_file
  in
  cmd

type xml_tla_channels =
  | Xml_channel of in_channel
  | TLA_channel of in_channel * out_channel * in_channel

let rec dump_channel c =
  let line = input_line c in
  Printf.printf "%s\n" line;
  dump_channel c;
  ()

let load_sany settings =
  let tla_error = "TLA to XML conversion error" in
  let fds = match settings.xml_input with
    | true  ->
       (* load sany xml ast from file *)
       let channel = open_in settings.input_file in
       Xml_channel channel
    | false ->
       (* load tla file from file calling java *)
       let env = Unix.environment () in
       let (jin, jout, jerr) =
         Unix.open_process_full (java_cmd settings) env in
       TLA_channel (jin, jout, jerr)
  in
  let channel = match fds with
    | Xml_channel c -> c
    | TLA_channel (c, _, _) -> c
  in
  let sany_context =
    try
      Ok (Sany.import_xml channel)
    with
    | e ->
       Error e
  in
  (* close_channels *)
  let exit_code = match fds with
    | Xml_channel c ->
       close_in c;
       Unix.WEXITED 0
    | TLA_channel (stdin, stdout, stderr) ->
       Unix.close_process_full (stdin, stdout, stderr)
  in
  match (exit_code, sany_context) with
  | Unix.WEXITED 0,    Ok sc -> sc
  | Unix.WEXITED code, _ ->
     let msg = asprintf "%s: java return code is %d"
                        tla_error code
         in
         failwith msg
      | _, Error e ->
         raise e
      | _, _ ->
         let msg = asprintf "%s: unknown error calling java process."
                 tla_error
         in
         failwith msg

let compute_obligations settings sany_context =
      (* extract builtins from file *)
      let sany_builtins =
        Sany_builtin_extractor.extract_from_context sany_context in
      (* convert sany ast to internal ast (expr_ds) *)
      let exprds =
        Sany_expr.convert_context ~builtins:sany_builtins sany_context in
      (* replace definitions of name LAMBDA by lambda constructors *)
      let fixed_lambda =
        Expr_correct_lambda.correct_lambda_context exprds in
      (* make language elements represented as builtin operators explicit *)
      let fixed_theorems =
        Expr_parse_theorems.expr_parse_theorems_context fixed_lambda in
      (* extract obligations *)
      Extract_obligations.extract_obligations_context fixed_theorems

let announce_obligations settings formatter obligations =
      (* print obligations to stdout *)
      ignore(
          List.fold_left (fun no (obl:obligation) ->
              let r = {id=no; location=obl.location; status = ToBeProved;
                       prover = None; meth=None; already_processed = Some false;
                       obligation_string = None } in
              fprintf formatter "%a@,@." fmt_toolbox_msg r;
              (*          fprintf std_formatter "%d @ %a@." no fmt_location obl.location; *)
              no+1
            ) 1 obligations
        );
      (* print obligation count message *)
      let obl_no = List.length obligations in
      fprintf formatter "@[<v>@,%a@]@." fmt_toolbox_msg_count obl_no;
      ()

let announce_all_failed settings formatter obligations =
      (* print obligation fail messages to stdout *)
      ignore(
          List.fold_left (fun no obl ->
              (*      fprintf std_formatter "Obligation %d:\n%a\n\n" no
              Obligation_formatter.fmt_obligation obl;*)
              let obligation_string =
                Some (asprintf "%a" Obligation_formatter.fmt_obligation obl) in
              let r = {id=obl.id; location=obl.location; status = Failed;
                       prover = Some Tlaps; meth=None;
                       already_processed = Some false; obligation_string } in
              fprintf err_formatter "%a@,@." fmt_toolbox_msg r;
              no+1
            ) 1 obligations
        );
      ()

type exit_status = Exit_status of int

let nunchaku_backend obligations settings =
  let f obligation =
    try
      let result = Nunchaku.nunchaku settings obligation obligation.id in
      match result with
      | Nun_mod.SAT _ ->
         let result_string = Nunchaku.nunchaku_result_printer (result) in
         let toolbox_msg = {
             id       = obligation.id;
             location = obligation.location;
             status   = Failed;
             prover   = Some Nunchaku;
             meth     = None;
             already_processed = Some false;
             obligation_string = Some result_string;
           }
         in
         print_string "\n\n";
         fmt_toolbox_msg err_formatter toolbox_msg
      | _ -> ()
    with
    | UnhandledLanguageElement (_, _) ->
       () (* fail gracefully for unhandled constructs *)
  in
  let clear_tmp = Printf.sprintf "rm '%s'/nunchaku/tmp*.*" settings.pm_path in
  ignore(Sys.command clear_tmp);
  ignore (List.map f (List.rev obligations))

let init () =
  Printexc.record_backtrace true;
  try begin
      let settings = handle_arguments Sys.argv in
      let sany_context = load_sany settings in
      let obligations = compute_obligations settings sany_context in
      announce_obligations settings err_formatter obligations;
      (* here goes the calling of backends *)
      nunchaku_backend obligations settings;

      Exit_status 0
    end
  with
  | (Xmlm.Error ((line,col), error)) ->
     Printf.printf "Xmlm error at position %d:%d with error %s.\n" line col
                    (Xmlm.error_message error);
     Printf.printf "Backtrace: %s\n\n" (Printexc.get_backtrace ());
     Exit_status 1
  | x ->
     Printf.printf "Exception: %s\n" (Printexc.to_string x);
     Printf.printf "Backtrace: %s\n\n" (Printexc.get_backtrace ());
     Exit_status 1
;;

let Exit_status n = init () in
    exit n;;
