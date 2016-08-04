open Commons
open Format
open Util
open List

type settings = {
  (* SANY/XML related *)
  java_executable : string;
  check_schema    : bool;
  xml_input       : bool;
  include_paths   : string list;

  (* pm settings *)
  verbose      : bool;
  overlord     : bool;
  toolbox      : int_range;
  fingerprints : bool;
  input_file   : string;
  pm_path      : string;

  (* unchaku backend settings *)
  models_in_tla : bool;
  nunchaku_executable : string;
}

let default_settings =
  {
    (* SANY/XML related *)
    java_executable = "java";
    check_schema    = false;
    xml_input       = false;
    include_paths   = [];

    (* pm settings *)
    toolbox      = { rbegin = 0; rend = 0};
    verbose      = false;
    overlord     = true;       (* TODO change back to false *)
    fingerprints = false;
    input_file   = "";
    pm_path      = autodetect_executable_path ;

    (* unchaku backend settings *)
    models_in_tla = true;
    nunchaku_executable = "nunchaku";
  }

let fmt_settings formatter { java_executable; check_schema; xml_input; include_paths;
                             verbose; overlord; toolbox; fingerprints; input_file;
                             pm_path; models_in_tla; nunchaku_executable } =
  fprintf formatter "@[<v 2>{@,";
  fprintf formatter "java exec     = %s@," java_executable;
  fprintf formatter "verbose       = %b@," verbose;
  fprintf formatter "overlord      = %b@," overlord;
  fprintf formatter "check schema  = %b@," check_schema;
  fprintf formatter "xml input     = %b@," xml_input;
  map (fprintf formatter "include directory: %s@,") include_paths;
  fprintf formatter "toolbox range = %a@," fmt_int_range toolbox;
  fprintf formatter "fingerprints  = %b@," fingerprints;
  fprintf formatter "input file    = \"%s\"@," input_file;
  fprintf formatter "tlapm binary location  = \"%s\"@," pm_path;
  fprintf formatter "print nunchaku models in TLA syntax  = \"%b\"@," models_in_tla;
  fprintf formatter "nunchaku exec = %s@," nunchaku_executable;
  fprintf formatter "@]}";
  ()
