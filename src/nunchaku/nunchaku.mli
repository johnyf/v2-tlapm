open Tla_pb
open Tla_simple_pb
open Nun_pb
open Nun_pb_fmt
open Nun_sexp
open Nun_mod
open Tla_mod
       
type nunchaku_result = tla_mod
                      
val nunchaku_result_printer : nunchaku_result -> string option
       
val call_nunchaku : statement list -> Settings.settings -> int -> nun_sexp

val nunchaku : Settings.settings -> tla_pb -> int -> nunchaku_result
