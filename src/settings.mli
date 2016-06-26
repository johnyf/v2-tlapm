open Commons
open Format

(** This module represents the global settings for the PM. *)

type settings = {
    (* SANY/XML related *)
    java_path    : string option; (* location of the java executable *)
    check_schema : bool;   (* check if the xm file conforms to the sany.xsd schema *)
    xml_input    : bool;   (* expect an xml input file instead of tla *)
    include_paths : string list; (* list of include directories *)

    (* pm settings *)
    verbose      : bool;
    toolbox      : int_range;
    fingerprints : bool;
    input_file   : string;
  }

val default_settings : settings
val fmt_settings : formatter -> settings -> unit
