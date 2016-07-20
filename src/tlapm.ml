open Commons
open Util
open Sany
open Obligation
open Extract_obligations
open Format
open Nunchaku
open Simple_obligation_formatter
open Expr_simple
open Expr_substitution
open Simple_obligation
open Simple_expr_prover_parser
open Sexp
open Mod
open Settings
open Arg_handler
open Toolbox
open Result


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

let handle_abort _ =
  if !Params.verbose then
    Util.eprintf ~prefix:"FATAL: " ">>> Interrupted -- aborting <<<" ;
  if !Params.stats then
    Clocks.report () ;
  Pervasives.exit 255
 *)


let tla_to_xml tla_filename xml_filename =
  let tla_to_xml = "sh tla2xml.sh -o -I ./library/ "^tla_filename^" > "^xml_filename in
  ignore(Sys.command tla_to_xml)

let xml_to_obl xml_filename output_file=
  let channel = open_in xml_filename in
  (* load sany xml ast from file *)
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
  (*
     ignore(
         List.fold_left (fun no obl ->
                         fprintf std_formatter "Obligation %d:\n%a\n\n" no
                                 Obligation_formatter.fmt_obligation obl;
                         no+1
                        ) 1 obligations
       )
   *)
  (* call nunckaku *)
  (* print_complex obligations "nun/complex.txt"; *)
  print_simple obligations output_file;
  obligations

let obl_to_nun obligations dir =
  (* Directory in which the .nun files will be created. One file per obligation. *)
  (* The directory needs to exist, otherwise it won't work. *)
  (* TODO Add a command to create the directory if it doesn't exist. *)
  print_nunchaku obligations dir

let nun_to_sexp nun_file sexp_file =
  let nunk = "nunchaku -o sexp "^nun_file^" > "^sexp_file in
  ignore(Sys.command nunk)
		  
let print_nun k =
  let sk = "echo \"\n----- OBLIGATION "^(string_of_int k)^": -----\n\"" in
  ignore(Sys.command sk) ;
  let nunk = "nunchaku nun/nun/"^(string_of_int k)^".nun" in
  ignore(Sys.command nunk)

let rec call_nun k = match k with
  | 0 -> ();
  | _ -> call_nun (k-1) ;
	 nun_to_sexp ("nun/nun/"^(string_of_int k)^".nun") ("nun/sexp/"^(string_of_int k)^".sexp")

let sexp_to_mod sexp_file mod_file =
  Mod.print_mod_tree mod_file (Mod.sexp_to_mod_tree (Sexp.sexp_parser sexp_file))
  		     
let rec convert_to_mod k = match k with
  | 0 -> ();
  | _ -> convert_to_mod (k-1);
	 sexp_to_mod ("nun/sexp/"^(string_of_int k)^".sexp") ("nun/mod/"^(string_of_int k)^".mod")


let print_some obligations n =
  let f obl k =
    let sk = "echo \"\n----- OBLIGATION "^(string_of_int k)^": -----\n\"" in
    ignore(Sys.command sk) ;
    fprintf std_formatter "%a" Obligation_formatter.fmt_obligation obl ;
    print_newline ();
    let sk = "echo \"\n----- COUNTEREXAMPLE "^(string_of_int k)^": -----\n\"" in
    ignore(Sys.command sk) ;
    let nunk = "cat nun/mod/"^(string_of_int k)^".mod" in
    ignore(Sys.command nunk);
  in
  let rec map_bis f l n = match l with
    | [] -> []
    | hd::tl -> let t = (f hd n) in t::(map_bis f tl (n+1))
  in
  ignore (map_bis f obligations 1)

let print_all obligations n =
  let f obl k =
    let sk = "echo \"\n----- OBLIGATION "^(string_of_int k)^": -----\n\"" in
    ignore(Sys.command sk) ;
    fprintf std_formatter "%a" Obligation_formatter.fmt_obligation obl ;
    print_newline ();
    let sk = "echo \"\n----- NUNCHAKU "^(string_of_int k)^": -----\n\"" in
    ignore(Sys.command sk) ;
    let nunk = "cat nun/nun/"^(string_of_int k)^".nun" in
    ignore(Sys.command nunk);
    let sk = "echo \"\n----- SEXP "^(string_of_int k)^": -----\n\"" in
    ignore(Sys.command sk) ;
    let nunk = "cat nun/sexp/"^(string_of_int k)^".sexp" in
    ignore(Sys.command nunk);
    let sk = "echo \"\n----- MOD "^(string_of_int k)^": -----\n\"" in
    ignore(Sys.command sk) ;
    let nunk = "cat nun/mod/"^(string_of_int k)^".mod" in
    ignore(Sys.command nunk);
  in
  let rec map_bis f l n = match l with
    | [] -> []
    | hd::tl -> let t = (f hd n) in t::(map_bis f tl (n+1))
  in
  ignore (map_bis f obligations 1)



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
      List.fold_left (fun no (obl:obligation) ->
          let r = {id=no; location=obl.location; status = ToBeProved;
                   prover = None; meth=None; already_processed = Some false;
                   obligation_string = None } in
          fprintf formatter "%a@,@." fmt_toolbox_msg r;
          (*          fprintf std_formatter "%d @ %a@." no fmt_location obl.location; *)
          no+1
        ) 1 obligations;
      (* print obligation count message *)
      let obl_no = List.length obligations in
      fprintf formatter "@[<v>@,%a@]@." fmt_toolbox_msg_count obl_no;
      ()

let announce_all_failed settings formatter obligations =
      (* print obligation fail messages to stdout *)
      List.fold_left (fun no obl ->
          (*      fprintf std_formatter "Obligation %d:\n%a\n\n" no
              Obligation_formatter.fmt_obligation obl;*)
          let obligation_string =
            Some (asprintf "%a" Obligation_formatter.fmt_obligation obl) in
          let r = {id=no; location=obl.location; status = Failed;
                   prover = Some Tlaps; meth=None;
                   already_processed = Some false; obligation_string } in
          fprintf err_formatter "%a@,@." fmt_toolbox_msg r;
          no+1
        ) 1 obligations;
      ()

type exit_status = Exit_status of int

let nunchaku_backend settings obligations =
  let filename = "dummy" in
  match settings.models_in_tla with
  | true ->
     begin
     let n = obl_to_nun obligations "nun/nun" in
       call_nun (n-1) ;
       convert_to_mod (n-1) ;
       match settings.verbose with
       | true ->
          print_all obligations (n-1); print_newline ()
       |false ->
         print_some obligations (n-1); print_newline ()
     end
  | false ->
     begin
       ignore(sexp_to_mod ("nun/sexp/"^filename^".sexp") ("nun/mod/"^filename^".mod"));
     end
  ;;

let init () =
  Printexc.record_backtrace true;
  try begin
      let settings = handle_arguments Sys.argv in
      if settings.verbose then
        begin
          Format.fprintf Format.std_formatter "%a@." fmt_settings settings;
          Format.fprintf Format.std_formatter "call command: %s@."
                         (java_cmd settings);
        end;
      let sany_context = load_sany settings in
      let obligations = compute_obligations settings sany_context in
      announce_obligations settings err_formatter obligations;
      (* here goes the calling of backends *)
      
      Exit_status 0
    end
  with
  | (Xmlm.Error ((line,col), error)) as x ->
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
