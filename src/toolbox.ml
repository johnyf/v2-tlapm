open Expr_ds
open Commons
open Format

(* This implements the datastructures for the toolbox communication protocol.
   See also general/docs/tlapm-toolbox-interface.txt in the toolbox code.
*)

type toolbox_status = | ToBeProved | Proved | Failed | Trivial
                      | BeingProved | Interrupted

(* TODO: how many proofs actually use that? *)
type toolbox_method = | Auto | Blast | Force | Fail | Sorry

type toolbox_msg = {
    id       : int;                      (* obligation id *)
    location : location;                 (* location of statement to prove *)
    status   : toolbox_status;           (* proving status of the obligation *)
    prover   : prover option;            (* which prover was used *)
    meth     : toolbox_method option;    (* which method was passed *)
    already_processed : bool option;     (* fingerprint used *)
    obligation_string  : string option;
  }

let toolbox_status_string = function
  | ToBeProved -> "to be proved"
  | Proved -> "proved"
  | Failed -> "failed"
  | Trivial -> "trivial"
  | BeingProved -> "being proved"
  | Interrupted -> "interrupted"

let toolbox_method_string = function
  | Auto -> "auto"
  | Blast -> "blast"
  | Force -> "force"
  | Fail -> "fail"
  | Sorry -> "sorry"


let head = "@!!"
let front = "BEGIN"
let back = "END"

let fmt_toolbox_status formatter s =
  fprintf formatter "%s" (toolbox_status_string s);
  ()

let fmt_toolbox_method formatter s =
  fprintf formatter "%s" (toolbox_method_string s);
  ()

let prover_string = function
  | Isabelle -> "isabelle"
  | Zenon    -> "zenon"
  | SMT      -> "smt"
  | LS4      -> "ls4"
  | Default  -> ""

let fmt_prover f x =
  fprintf f "%s"(prover_string x)

let fmt_toolbox_msg_d formatter id location status ?prover:prover
                      ?meth:meth ?already_processed:already_processed
                      ?obligation_string:obligation_string =
  fprintf formatter "@[<v>%s%s@," head front;
  fprintf formatter "%stype:obligation@," head;
  fprintf formatter "%sid:%d@," head id;
  fprintf formatter "%sloc:%d:%d:%d:%d@," head
          location.line.rbegin location.column.rbegin
          location.line.rend location.column.rend;
  fprintf formatter "%sstatus:%a@," head fmt_toolbox_status status;
  begin
    match prover with
    | Some p -> fprintf formatter "%sprover:%a@," head fmt_prover p; ()
    | None -> ()
  end;
  begin
    match meth with
    | Some m -> fprintf formatter "%smethod:%a@," head fmt_toolbox_method m; ()
    | None -> ()
  end;
  begin
    match already_processed with
    | Some a -> fprintf formatter "%salready:%b@," head a; ()
    | None -> ()
  end;
  begin
    match obligation_string with
    | Some os -> fprintf formatter "%sobl:%s@," head os; ()
    | None -> ()
  end;
  fprintf formatter "%s%s@]" head back;
  ()

let fmt_toolbox_msg formatter { id; location; status; prover;
                                meth; already_processed; obligation_string } =
  fmt_toolbox_msg_d formatter id location status ?prover
                    ?meth ?already_processed ?obligation_string

let fmt_toolbox_msg_count formatter no =
  fprintf formatter "@[<v>%s%s@," head front;
  fprintf formatter "%stype:obligationsnumber@," head;
  fprintf formatter "%scount:%d@," head no;
  fprintf formatter "%s%s@]" head back;
  ()
