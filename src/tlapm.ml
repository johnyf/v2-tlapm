open Commons
open Util
open Sany
open Format
open Settings
open Arg_handler
open Result
open Obligation
open Obligation_formatter
open Extract_obligations
open Expr_substitution
open Expr_termdb_utils
open Toolbox
open Scheduler
open Backend_exceptions
open Isabelle

let  global_settings = ref default_settings

(** Creates the command line string used to invoke the sany parser *)
let java_cmd { check_schema; java_executable; include_paths;
               input_file; pm_path; _  } =
  let fmt_include = fmt_list ~front:"" ~middle:""
      ~back:""
      (fun fmt s -> fprintf fmt "-I \"%s\" " s) in
  let fmt_offline = if check_schema then "" else "-o" in
  let cmd = asprintf "%s -jar %s/lib/sany.jar %s %a \"%s\""
      java_executable pm_path fmt_offline  fmt_include include_paths
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
    (*    try *)
      Ok (Sany.import_xml channel)
(*    with
    | e ->
      Error e *)
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
  | Unix.WEXITED code, _ when code <> 0 ->
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
  (* convert sany ast to internal ast (expr_ds) *)
  let exprds =
    Sany_expr.convert_context ~builtins:[] sany_context in
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
    let fmt_id =
      (fun f -> function | {id;_ } -> CCFormat.fprintf f "oid %d:" id) in
    let fmt_obl = fun f o -> CCFormat.fprintf f " %a" fmt_obligation o in
    let (pp_msg, pp_obl) = if settings.toolbox_output
      then (fmt_toolbox_msg, CCFormat.silent)
      else (fmt_id, fmt_obl)
    in
    List.fold_left (fun no (obl:obligation) ->
        let r = {id=no; location=obl.location; status = ToBeProved;
                 prover = None; meth=None; already_processed = Some false;
                 obligation_string = None } in
        fprintf formatter "%a%a@,@."
          pp_msg r
          pp_obl obl
        ;
        no+1
      ) 1 obligations
  );
  (* print obligation count message *)
  let obl_no = List.length obligations in
  if settings.toolbox_output then (
    fprintf formatter "@[<v>@,%a@]@." fmt_toolbox_msg_count obl_no;
    ()
  ) else
    ()

let announce_results msgs =
  IntMap.fold (fun _ -> fun m -> fun _ ->
      fmt_toolbox_msg err_formatter m) msgs ()

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


let prepare_backends settings () =
  let clear_tmp = Printf.sprintf "rm '%s'/nunchaku/tmp*.*" settings.pm_path in
  ignore(Sys.command clear_tmp)
type exit_status = Exit_status of int


let init () =
  Printexc.record_backtrace true;
  try begin
    let settings = handle_arguments Sys.argv in
    global_settings := settings;
    let sany_context = load_sany settings in
    let obligations = compute_obligations settings sany_context in
    announce_obligations settings err_formatter obligations;
    (* here goes the calling of backends *)
    prepare_backends settings (); (*TODO refactor *)
    let messages = scheduler settings obligations in
    announce_results messages;
    Exit_status 0
  end
  with
  | Xmlm.Error ((line,col), error) ->
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
