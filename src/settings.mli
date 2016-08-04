open Commons
open Format

(** This module represents the global settings for the PM. *)

type settings = {
  (* SANY/XML related *)
  java_executable : string; (* the string to call the java interpreter *)
  check_schema : bool;   (* check if the xm file conforms to the sany.xsd schema *)
  xml_input    : bool;   (* expect an xml input file instead of tla *)
  include_paths : string list; (* list of include directories *)

  (* pm settings *)
  verbose      : bool;
  overlord     : bool;
  toolbox      : int_range;
  fingerprints : bool;
  input_file   : string;
  pm_path      : string;

  (* unchaku backend settings *)
  models_in_tla : bool;
  nunchaku_executable : string; (* the string to call nunchaku *)
}

val default_settings : settings
val fmt_settings : formatter -> settings -> unit
