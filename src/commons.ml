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
  | VariableLevel
  | ActionLevel
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

let mkDummyRange = { rbegin = 0; rend = 0 }
let mkDummyLocation = { column = mkDummyRange ;
                        line = mkDummyRange;
                        filename = "Dummy" }
