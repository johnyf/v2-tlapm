(* Copyright (C) 2014 MSR-INRIA
 * Author: TL
 *)

(** The SANY XML parser.

  It parses an xml file according to the xsd in lib/sany.xsd and creates
  datastructures from the {!module:Sany_ds}, which directly mirror the schema.

*)
open Commons
open Xmlm
open Sany_ds

val import_xml : Pervasives.in_channel -> context
