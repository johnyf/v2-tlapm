open Tla_pb
open Tla_simple_pb
open Nun_pb
open Nun_pb_fmt
open Nun_sexp
open Nun_mod
open Settings
       
type nunchaku_result = nun_mod
let nunchaku_result_printer = nun_mod_to_string

let call_nunchaku nun_pb_ast settings id =
  let path = settings.pm_path ^ "/nunchaku/" in
  let nun_file = path^"tmp_nun_pb_"^(string_of_int id)^".nun" in
  let sexp_file = path^"tmp_nun_sexp_"^(string_of_int id)^".txt" in
  let oc = open_out nun_file in
  let fft = Format.formatter_of_out_channel oc in
  Format.fprintf fft "%a@." print_statement_list nun_pb_ast;
  close_out oc;
  let call = Printf.sprintf "nunchaku -s cvc4 -o sexp '%s' > '%s' " nun_file sexp_file in (* TODO: add timeout *)
  ignore(Sys.command call);
  let nun_sexp_ast = sexp_parser sexp_file in
  if (not(settings.overlord))
  then
    begin
      ignore(Sys.command ("rm "^nun_file));
      ignore(Sys.command ("rm "^sexp_file));
    end
  else
    ();
  nun_sexp_ast
    
              
let nunchaku settings obligation id =
  let path = settings.pm_path ^ "/nunchaku/" in
  let tla_pb = obligation in
  if (settings.overlord)
  then
    print_tla_pb (path^"tmp_tla_pb_"^(string_of_int id)^".txt") tla_pb ;
  let tla_simple_pb = tla_pb_to_tla_simple_pb tla_pb in
  if (settings.overlord)
  then
    print_tla_simple_pb (path^"tmp_tla_simple_pb_"^(string_of_int id)^".txt") tla_simple_pb ;
  let nun_pb = simple_obl_to_nun_ast tla_simple_pb in
  let nun_sexp = call_nunchaku nun_pb settings id in
  let nun_mod = nun_sexp_to_nun_mod nun_sexp in
  if (settings.overlord)
  then
    print_nun_mod (path^"tmp_nun_mod_"^(string_of_int id)^".txt") nun_mod ;
  nun_mod

    

