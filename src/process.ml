(** Models the run of an external process *)
type 'a exec_result = Ok of 'a | Error of int * 'a
type 'a processor = in_channel -> unit -> 'a

let call_exec settings tool cmd process_out =
  let (channel, stdin, stderr) =
    let env = Unix.environment () in
    let (jin, jout, jerr) =
      Unix.open_process_full cmd env in
    (jin, jout, jerr)
  in
  let result = process_out channel () in
  (* close_channels *)
  let exit_code =
    Unix.close_process_full (channel, stdin, stderr)
  in
  match exit_code with
  | Unix.WEXITED 0 -> Ok result
  | Unix.WEXITED code -> Error (code, result)
  | Unix.WSIGNALED code ->
    let msg = Format.asprintf "process killed with signal %d" code in
    raise (Backend_exceptions.ExternalToolFailed (tool, msg, ""))
  | Unix.WSTOPPED code ->
    let msg = Format.asprintf "process stopped with signal %d" code in
    raise (Backend_exceptions.ExternalToolFailed (tool, msg, ""))

let process_stdout c () =
  let buffer = Buffer.create 16 in
  let rec aux c =
    try
      let line = input_line c in
      Buffer.add_string buffer line;
      aux c;
    with
    | End_of_file ->
      ()
  in
  aux c;
  Buffer.contents buffer
