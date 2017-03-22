(** Models the run of an external process *)
type 'a exec_result = Ok of 'a | Error of int * 'a
type 'a processor = in_channel -> unit -> 'a

val call_exec : Settings.settings -> Commons.prover -> string
  -> 'a processor -> 'a exec_result
(** [call_exec settings prover cmd processor] opens a shell, connects
    to stdin, stdout and stderr and executes. It calls processor on stdin
    and returns its result, of the command executes with exit code 0. A regular
    closure with error code different from 0 results in an Error with the error
    code and the result.
*)

val process_stdout : string processor
(** Processes all output of a childprocess and returns the string. *)
