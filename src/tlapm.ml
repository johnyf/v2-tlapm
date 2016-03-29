open Sany
open Tlapm_args
open Obligation
open Extract_obligations
open Format
open Nunchaku
open Simple_obligation_formatter
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
let java_cmd offline search_path input_files = "java -jar lib/sany.jar" ^
  (if !offline then " -o " else "") ^ (* add offline flag, if neccessary *)
    (if (List.length search_path > 0) then
       " -I " ^ (String.concat " -I " search_path) (* add include directories*)
     else "")
    ^ " " ^ (String.concat " " input_files) (* add input file *)


let init () =
  (* argument handling TODO: rewrite *)
  match  Array.length Sys.argv with
  | 2 ->
     let filename = Sys.argv.(1) in
     let channel = open_in filename in
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
     (* call nunckaku *)
     ignore(nunchaku obligations)
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
  | _ ->
     Printf.eprintf "TLAPM does no argument handling right now.\n";
     Printf.eprintf "Syntax: ./tlapm.byte file.xml\n";
     ()
;;


init ();;
