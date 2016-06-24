open Commons
open Format

type settings = {
    (* SANY/XML related *)
    java_path    : string;
    check_schema : bool;
    xml_input      : bool;

    (* pm settings *)
    toolbox      : int_range;
    fingerprints : bool;
    input_file   : string;
  }

let default_settings =
  {
    (* SANY/XML related *)
    java_path    = "";
    check_schema = false;
    xml_input      = true;

    (* pm settings *)
    toolbox      = { rbegin = 0; rend = 0};
    fingerprints = false;
    input_file   = "";
  }

let fmt_settings formatter { java_path; check_schema; xml_input;
                             toolbox; fingerprints; input_file } =
  fprintf formatter "@[<v 2>{@,";
  fprintf formatter "java path     = \"%s\"@," java_path;
  fprintf formatter "check schema  = %b@," check_schema;
  fprintf formatter "xml input     = %b@," xml_input;
  fprintf formatter "toolbox range = %a@," fmt_int_range toolbox;
  fprintf formatter "fingerprints  = %b@," fingerprints;
  fprintf formatter "input file    = \"%s\"@," input_file;
  fprintf formatter "@]}";
  ()
