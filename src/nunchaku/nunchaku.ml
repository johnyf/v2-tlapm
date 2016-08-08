open Commons
open Tla_pb
open Tla_simple_pb
open Nun_pb
open Nun_pb_fmt
open Nun_sexp
open Nun_mod
open Tla_mod
open Settings
open Backend_exceptions

type nunchaku_result = tla_mod

let nunchaku_result_printer result = match result with
  | REFUTED _ -> Some (tla_mod_to_string result)
  | _         -> None

let buffer_input nunchaku_out =
  let buffer = Buffer.create 16 in
  begin
    try
      while true do
        input_line nunchaku_out |> Buffer.add_string buffer;
        Buffer.add_char buffer '\n';
      done
    with End_of_file ->
      ()
  end;
  Buffer.contents buffer

let call_nunchaku nun_pb_ast settings id =
  let path = settings.nunchaku_temp_path in
  let nun_file = Printf.sprintf "%s/tmp_nun_pb_%d.nun" path id in
  let sexp_file = Printf.sprintf "%s/tmp_nun_sexp_%d.txt" path id in
  let oc = open_out nun_file in
  let fft = Format.formatter_of_out_channel oc in
  Format.fprintf fft "%a@." print_statement_list nun_pb_ast;
  close_out oc;
  let call = Printf.sprintf "%s -s cvc4 -o sexp --timeout 10 '%s' > '%s' "
      settings.nunchaku_executable nun_file sexp_file in
  let env = Unix.environment () in
  let (nout, nin, nerr) = Unix.open_process_full call env in
  let model_buffer = buffer_input nout in
  let err_buffer = buffer_input nerr in
  let return_code = Unix.close_process_full (nout, nin, nerr) in
  match return_code with
  | Unix.WEXITED 0 ->
    let nun_sexp_ast = sexp_parser sexp_file in
    if (not(settings.overlord))
    then
      begin
        ignore(Sys.command ("rm "^nun_file));
        ignore(Sys.command ("rm "^sexp_file));
      end;
    nun_sexp_ast
  | Unix.WEXITED n ->
    let msg = Format.asprintf "Nunchaku failed with error code %d." n in
    raise (ExternalToolFailed (Nunchaku, msg, err_buffer))
  | Unix.WSTOPPED n ->
    let msg = Format.asprintf "Sub-process was halted by signal number %d." n in
    raise (ExternalToolFailed (Nunchaku, msg, err_buffer))
  | Unix.WSIGNALED n ->
    let msg = Format.asprintf "Sub-process was halted by signal number %d." n in
    raise (ExternalToolFailed (Nunchaku, msg, err_buffer))


let nunchaku settings obligation id =
  let path = settings.pm_path ^ "/nunchaku/" in
  let tla_pb = obligation in
  if (settings.overlord)
  then
    print_tla_pb (path^"tmp_tla_pb_"^(string_of_int id)^".txt") tla_pb ;
  let tla_simple_pb = tla_pb_to_tla_simple_pb tla_pb in
  if (settings.overlord)
  then
    print_tla_simple_pb (path^"tmp_tla_simple_pb_"^(string_of_int id)^".txt")
      tla_simple_pb ;
  let nun_pb = tla_simple_pb_to_nun_ast tla_simple_pb in
  let nun_sexp = call_nunchaku nun_pb settings id in
  let nun_mod = nun_sexp_to_nun_mod nun_sexp in
  if (settings.overlord)
  then
    print_nun_mod (path^"tmp_nun_mod_"^(string_of_int id)^".txt") nun_mod ;
  let tla_mod = nun_mod_to_tla_mod nun_mod in
  if (settings.overlord)
  then
    print_tla_mod (path^"tmp_tla_mod_"^(string_of_int id)^".txt") tla_mod ;
  tla_mod
