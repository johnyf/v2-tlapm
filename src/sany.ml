(* Copyright (C) 2014 MSR-INRIA
 *
 * SANY expressions
 *
 * Author: TL
 *)

open Commons

type op_decl = {
  a: string
}

and op_def = {
  b: string
}

and assumption = {
  c: string
}

and theorem = {
  d: string
}

(* modules *)
and mule = {
  name              : string;
  loc               : location;
  constants         : op_decl list;
  variables         : op_decl list;
  definitions       : op_def list ;
  assumptions       : assumption list ;
  theorems          : theorem list ;
}
