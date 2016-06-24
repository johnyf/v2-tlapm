open Settings
open Arg
open Commons

type state = { mutable settings : settings }


let handle_fingerprints state b =
  state.settings <- { state.settings with fingerprints = b };
  ()

let handle_toolbox_lower state i =
  let toolbox = { state.settings.toolbox with rbegin = i } in
  state.settings <- { state.settings with toolbox };
  ()

let handle_toolbox_upper state i =
  let toolbox = { state.settings.toolbox with rend = i } in
  state.settings <- { state.settings with toolbox };
  ()

let handle_rest state s =
  state.settings <- { state.settings with input_file = s }

let handlers s =
  [
    ("-fingerprints", Bool (handle_fingerprints s),
     "Use fingerprinting (default: no)");
    ("-toolbox", Tuple [Int (handle_toolbox_lower s);
                        Int (handle_toolbox_upper s)],
     "Output in toolbox mode from first argument to second argument"
    )
  ]

let use_string =
  ("usage: tlapm [-fingerprints] [-xml_input] [-java_path path]" ^
    "[-toolbox from to] input_file")

let handle_arguments argv =
  let s = { settings = default_settings } in
  parse (handlers s) (handle_rest s) use_string;
  match  s.settings.input_file with
  | "" ->
     usage  (handlers s) use_string;
     failwith "Missing input file!"
  | _ ->
     s.settings
