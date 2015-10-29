(* Copyright (C) 2014 MSR-INRIA
 * Author: TL
 *)

(**
  Tlapm datatypes are constructed in layers,
  where each layer is obtained from a previous one
  by the use of a transformation.
  The first layer is obtained from SANY and the last one
  contains the obligations shipped to the backends.

  This file contains common definitions shared by all layers.
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

type prover =
  | Isabelle
  | Zenon
  | SMT
  | LS4
  | Default

(** Creates a range from 0 to 0. *)
val mkDummyRange : int_range

(** Creates a location at line 0 to 0, column 0 to 0. *)
val mkDummyLocation : location

val format_location : location -> string
