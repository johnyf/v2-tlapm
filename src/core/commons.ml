(* Copyright (C) 2014 MSR-INRIA
 *
 * tlapm datatypes are constructed in layers,
 * where each layer is obtained from a previous one
 * by the use of a transformation.
 * The first layer is obtained from SANY and the last one
 * contains the obligations shipped to the backends.
 *
 * This file contains common definitions shared by all layers.
 *
 *
 * Author: TL
*)
open Format
open Result

type int_range = {
  rbegin  : int;
  rend    : int
}

type location = {
  column  : int_range;
  line    : int_range;
  filename: string
}

type level =
  | ConstantLevel
  | StateLevel
  | TransitionLevel
  | TemporalLevel

type op_decl_kind =
  | ConstantDecl
  | VariableDecl
  | BoundSymbol
  | NewConstant
  | NewVariable
  | NewState
  | NewAction
  | NewTemporal

type prover =
  | Isabelle
  | Zenon
  | SMT
  | LS4
  | Tlaps
  | Nunchaku
  | Default

let mkDummyRange = { rbegin = 0; rend = 0 }
let mkDummyLocation = { column = mkDummyRange ;
                        line = mkDummyRange;
                        filename = "*** Dummy ***" }
let toplevel_loation = { column = mkDummyRange ;
                         line = mkDummyRange;
                         filename = "*** toplevel ***" }

let format_location {filename; column; line} =
  filename ^ ":"
  ^ (string_of_int line.rbegin) ^ ","
  ^ (string_of_int column.rbegin) ^ "-"
  ^ (string_of_int line.rend) ^ ","
  ^ (string_of_int column.rend)

let fmt_location fmt l =
  Format.fprintf fmt "%s" (format_location l)

let fmt_int_range f { rbegin; rend; } =
  fprintf f "@[%d-%d@]" rbegin rend;
  ()

let fmt_location f { filename = fn;
                     column   = c;
                     line     =  l;
                   } =
  fprintf f "@[%s:%d,%d-%d,%d@]" fn l.rbegin c.rbegin l.rend c.rend;
  ()

let format_prover = function
  | Isabelle -> "Isabelle"
  | Zenon -> "Zenon"
  | SMT -> "SMT"
  | LS4 -> "LS4"
  | Tlaps -> "TLAPS"
  | Nunchaku -> "Nunchaku"
  | Default -> "Default provers"

let fmt_prover f p =
  fprintf f "%s" (format_prover p)

let format_op_decl_kind = function
  | ConstantDecl -> "constant"
  | VariableDecl -> "variable"
  | BoundSymbol -> "bound symbol"
  | NewConstant -> "new constant"
  | NewVariable -> "new variable"
  | NewState -> "new state"
  | NewAction -> "new action"
  | NewTemporal -> "new temporal"

let lmax = function
  | ConstantLevel ->
    (function x -> x)
  | StateLevel ->
    (function
      | ConstantLevel
      | StateLevel -> StateLevel
      | x -> x
    )
  | TransitionLevel ->
    (function
      | ConstantLevel
      | StateLevel
      | TransitionLevel -> TransitionLevel
      | x -> x
    )
  | TemporalLevel ->
    (function _ -> TemporalLevel)

let level_of_op_decl_kind = function
  | ConstantDecl -> Ok ConstantLevel
  | VariableDecl -> Ok StateLevel
  | NewConstant  -> Ok ConstantLevel
  | NewVariable  -> Ok StateLevel
  | NewState     -> Ok StateLevel
  | NewAction    -> Ok TransitionLevel
  | NewTemporal  -> Ok TemporalLevel
  | BoundSymbol  -> Error "bound symbol"
