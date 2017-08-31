open Obligation
open Nunchaku
open Isabelle
open Toolbox
open Backend_exceptions
open CCFormat

let run_nunchaku settings obligation =
  let f obligation =
    try
      let result = nunchaku_result_printer
          (nunchaku settings obligation obligation.id) in
      match result with
      | Some message ->
        let toolbox_msg = {
          id       = obligation.id;
          location = obligation.location;
          status   = Countermodel;
          prover   = Some Commons.Nunchaku;
          meth     = None;
          already_processed = Some false;
          obligation_string = Some message;
        }
        in
        print_string "\n\n";
        fmt_toolbox_msg Format.err_formatter toolbox_msg;
        Some toolbox_msg
      | None -> None
    with
    | UnhandledLanguageElement (_, _) ->
      None (* fail gracefully for unhandled constructs *)
  in
  f obligation

let run_isabelle settings obl =
  let file, channel =
    Filename.open_temp_file ?mode:(Some [Open_binary]) "isabelle" ".thy" in
  let fn = Filename.basename file |> Filename.chop_extension in
  let file_quoted = Filename.quote file in
  "isabelle tempfile: " ^ file  |> debug;
  let fmt = Format.formatter_of_out_channel channel in
  begin
    try
      fprintf fmt "%a@." fmt_isabelle (fn, obl)
    with
    | e ->
      close_out channel;
      raise e
  end;
  let cmd = sprintf "cat %s | isabelle tty -l TLA+ " file_quoted in
  debug ("executing: '"^cmd^"'");
  match Process.call_exec settings Commons.Isabelle cmd
          Process.process_stdout with
  | Process.Ok _ ->
    debug ("isabelle proved " ^ fn);
    Some Proved
  | Process.Error (n, m) ->
    let msg =
      Format.asprintf "isabelle failed for %s with code %d and msg\n%s" fn n m in
    debug msg;
    Some Failed
