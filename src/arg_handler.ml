open Settings
open Arg
open Commons
open List

type state = { mutable settings : settings }


let handle_fingerprints state () =
  state.settings <- { state.settings with fingerprints = true };
  ()

let handle_xml_input state () =
  state.settings <- { state.settings with xml_input = true };
  ()

let handle_check_schema state () =
  state.settings <- { state.settings with check_schema = true };
  ()

let handle_verbose state () =
  state.settings <- { state.settings with verbose = true };
  ()

let handle_overlord state () =
  state.settings <- { state.settings with overlord = true };
  ()

let handle_java_exec state str =
  state.settings <- { state.settings with java_executable = str };
  ()

let handle_nunchaku_exec state str =
  state.settings <- { state.settings with nunchaku_executable = str };
  ()

let handle_nunchaku_temp_path state str =
  state.settings <- { state.settings with nunchaku_temp_path = str };
  ()

let handle_include_path state str =
  state.settings <- { state.settings with
                      include_paths = str :: state.settings.include_paths };
  ()

let handle_toolbox_lower state i =
  let toolbox = { state.settings.toolbox with rbegin = i } in
  let toolbox_output = true in
  state.settings <- { state.settings with toolbox; toolbox_output };
  ()

let handle_toolbox_upper state i =
  let toolbox = { state.settings.toolbox with rend = i } in
  let toolbox_output = true in
  state.settings <- { state.settings with toolbox; toolbox_output };
  ()

let handle_rest state s =
  state.settings <- { state.settings with input_file = s }

let handlers s =
  [
    ("--fingerprints", Unit (handle_fingerprints s),
     "Use fingerprinting (default: no)");
    ("--xml-input", Unit (handle_xml_input s),
     "Directly load a SANY xml file.");
    ("--isaprove", Unit (fun _ -> ()),
     "Compatibility argument."); (* TODO: check if really unneccessary *)
    ("--check-schema", Unit (handle_check_schema s),
     "Check the SANY xml against its schema (requires internet access).");
    ("--with-java", String (handle_java_exec s),
     "The java executable to use.");
    ("-I", String (handle_include_path s),
     "Add directory to include paths.");
    ("--toolbox", Tuple [Int (handle_toolbox_lower s);
                         Int (handle_toolbox_upper s)],
     "Output in toolbox mode from first argument to second argument");
    ("-v", Unit (handle_verbose s),
     "Enable verbose mode.");
    ("--overlord", Unit (handle_overlord s),
     "Enable overlord mode.");
    ("--with-nunchaku", String (handle_nunchaku_exec s),
     "The nunchaku executable to use.");
    ("--nunchaku-temp", String (handle_nunchaku_exec s),
     "The directory containing the nunchaku theories and temporary files.");
  ]

let use_string =
  ("usage: tlapm [--fingerprints] [--xml-input] [--with-java path]" ^
   "[--check-schema] [--toolbox from to] input_file")

let handle_arguments argv =
  let s = { settings = default_settings } in
  parse (handlers s) (handle_rest s) use_string;
  match  s.settings.input_file with
  | "" ->
    usage  (handlers s) use_string;
    failwith "Missing input file!"
  | _ ->
    { s.settings with include_paths = rev s.settings.include_paths }
