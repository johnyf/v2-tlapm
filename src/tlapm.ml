open Commons
open Util
open Sany
open Obligation
open Extract_obligations
open Format
open Expr_substitution
open Settings
open Arg_handler
open Toolbox
(*
module Clocks = struct
  include Timing

  let parsing = new_clock "parsing"
  let print   = new_clock "formatting"
  let elab    = new_clock "analysis"
  let gen     = new_clock "generation"
  let prep    = new_clock "simplification"
  let backend = new_clock "interaction"
  let check   = new_clock "checking"
  let fp_loading = new_clock "fp_loading"
  let fp_saving = new_clock "fp_saving"
  let fp_compute = new_clock "fp_compute"

  let pad_left md str = Printf.sprintf "%*s" md str

  let report () =
    let clocks =
      [ total "total"; parsing; elab; gen; prep; print; backend; check;
        fp_loading; fp_saving; fp_compute; ambient ]
    in
    let max_desc_width =
      List.fold_left (fun mx cl -> max mx (String.length cl.desc)) 0 clocks
    in
    let clocks =
      List.map (fun cl -> {cl with desc = pad_left max_desc_width cl.desc})
               clocks
    in
    Util.printf "(* %s | time (seconds) *)"
                (pad_left max_desc_width "operation");
    Util.printf "(* %s-+--------------- *)" (String.make max_desc_width '-');
    List.iter begin
      fun cl -> Util.printf "(* %s  *)%!" (string_of_clock cl)
    end (List.tl clocks);
    Util.printf "(* %s-+--------------- *)" (String.make max_desc_width '-');
    Util.printf "(* %s  *)" (string_of_clock (List.hd clocks));

end
 *)


(*
let handle_abort _ =
  if !Params.verbose then
    Util.eprintf ~prefix:"FATAL: " ">>> Interrupted -- aborting <<<" ;
  if !Params.stats then
    Clocks.report () ;
  Pervasives.exit 255
 *)
(*
let main fs =
  Params.input_files := List.map Filename.basename fs;
  let () =
    List.iter begin
      fun s ->
        ignore (Sys.signal s (Sys.Signal_handle handle_abort))
    end [Sys.sigint ; Sys.sigabrt ; Sys.sigterm] in
  let () = Format.pp_set_max_indent Format.std_formatter 2_000_000 in
  (* import the xml *)
  let cmd = java_cmd Params.offline_mode !Params.rev_search_path fs in
  (*  let () = print_string cmd in *)
  let ic, oc = Unix.open_process cmd in
  let mds = import_xml ic in
  ()

let init () =
  Printexc.record_backtrace true;
  Format.pp_set_max_indent Format.err_formatter 35;
  if Config.debug then
    main (Tlapm_args.init ())
  else
    try main (Tlapm_args.init ()) with
    | Errors.Fatal ->
       Util.eprintf "tlapm: Exiting because of the above error.";
       exit 0;
    | e ->
       let backtrace = Printexc.get_backtrace () in
       Format.pp_print_flush Format.std_formatter ();
       Format.pp_print_flush Format.err_formatter ();
       Pervasives.flush stdout;
       Pervasives.flush stderr;
       let error = (Printexc.to_string e) ^ "\n" ^ backtrace in
       Util.eprintf ~prefix:"FATAL:" " tlapm ending abnormally with %s" error;
       let config = Params.print_config_toolbox false in
       begin match !Errors.loc,!Errors.msg with
       | Some l,Some m -> Toolbox.print_message (l ^  "\n\n" ^ m)
       | None, Some m -> Toolbox.print_message m
       | _,_ ->
          let msg =
            Printf.sprintf
               "Oops, this seems to be a bug in TLAPM.\n\
                Please give feedback to developers.\n\n\n %s\n%s"
               error config
          in
          let url = "http://tla.msr-inria.inria.fr/bugs" in
          Toolbox.print_message_url msg url;
       end;
       exit 3;
;;

exception Stacktrace;;

Sys.set_signal Sys.sigusr1 (Sys.Signal_handle (fun _ -> raise Stacktrace));;

init ();;
 *)


(** Creates the command line string used to invoke the sany parser *)
let java_cmd { check_schema; java_path; include_paths; input_file } =
  let fmt_path = fmt_option ~none:"java" ~some:"" ~some_back:"" fmt_string in
  let fmt_include = fmt_list ~front:"-I \"" ~middle:"\" -I \""
                             ~back:"\"" fmt_string in
  let fmt_offline = if check_schema then "" else "-o" in
  let bin_string = Array.get Sys.argv 0 in
  let lib_path= Str.global_replace (Str.regexp "/[^/]*$") "" bin_string in
  let cmd = asprintf "%a -jar %s/lib/sany.jar %s %a \"%s\""
                     fmt_path java_path
                     lib_path
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

let init () =
  Printexc.record_backtrace true;
  try begin
      let settings = handle_arguments Sys.argv in
      (*
       Format.fprintf Format.std_formatter "%a@." fmt_settings settings;
       Format.fprintf Format.std_formatter "call command: %s@." (java_cmd settings);
       *)
      let fds = match settings.xml_input with
        | true  ->
           (* load sany xml ast from file *)
           let channel = open_in settings.input_file in
           (* close_in channel; *)
           Xml_channel channel
        | false ->
           (* load tla file from file calling java *)
           let env = Unix.environment () in
           let (jin, jout, jerr) =
             Unix.open_process_full (java_cmd settings) env in
           (*           Unix.close_process_full fds; *)
           TLA_channel (jin, jout, jerr)
      in
      let channel = match fds with
        | Xml_channel c -> c
        | TLA_channel (c, _, _) -> c
      in
      let sany_context = Sany.import_xml channel in
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
      let obligations =
        Extract_obligations.extract_obligations_context fixed_theorems in
      (* print obligations to stdout *)
      List.fold_left (fun no (obl:obligation) ->
          let r = {id=no; location=obl.location; status = ToBeProved;
                   prover = None; meth=None; already_processed = Some false;
                   obligation_string = None } in
          fprintf err_formatter "%a@,@." fmt_toolbox_msg r;
          (*          fprintf std_formatter "%d @ %a@." no fmt_location obl.location; *)
          no+1
        ) 1 obligations;
      let obl_no = List.length obligations in
      fprintf err_formatter "@[<v>@,%a@]@." fmt_toolbox_msg_count obl_no;
      (* print obligation fail messages to stdout *)
      List.fold_left (fun no obl ->
          (*      fprintf std_formatter "Obligation %d:\n%a\n\n" no
              Obligation_formatter.fmt_obligation obl;*)
          let obligation_string =
            Some (asprintf "%a" Obligation_formatter.fmt_obligation obl) in
(*          let r = {id=no; location=obl.location; status = BeingProved; prover = Some Tlaps;
                   meth=None; already_processed = Some false; obligation_string } in
          fprintf err_formatter "%a@,@." fmt_toolbox_msg r;
 *)
          let r = {id=no; location=obl.location; status = Failed; prover = Some Tlaps;
                   meth=None; already_processed = Some false; obligation_string } in
          fprintf err_formatter "%a@,@." fmt_toolbox_msg r;
          no+1
        ) 1 obligations;
      0
    end
  with
  | (Xmlm.Error ((line,col), error)) as x ->
     Printf.printf "Xmlm error at position %d:%d with error %s.\n" line col
                    (Xmlm.error_message error);
     Printf.printf "Exception: %s\n" (Printexc.to_string x);
     Printf.printf "Backtrace: %s\n\n" (Printexc.get_backtrace ());
     1
  | x ->
     Printf.printf "Exception: %s\n" (Printexc.to_string x);
     Printf.printf "Backtrace: %s\n\n" (Printexc.get_backtrace ());
     1
;;

init () |> exit;;
