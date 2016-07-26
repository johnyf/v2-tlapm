open Tla_pb
open Tla_simple_pb
open Nun_pb
open Nun_pb_fmt
open Nun_sexp
open Nun_mod
       
type nunchaku_result = nun_mod
                      
val nunchaku_result_printer : nunchaku_result -> string
       
val call_nunchaku : statement list -> Settings.settings -> int -> nun_sexp

val nunchaku : Settings.settings -> tla_pb -> int -> nun_mod
