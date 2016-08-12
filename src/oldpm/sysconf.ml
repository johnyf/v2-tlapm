(*
 * sysconf.ml --- thin interface to POSIX.1 sysconf(2)
 *
 * Author: Kaustuv Chaudhuri <kaustuv.chaudhuri@inria.fr>
 *
 * Copyright (C) 2008-2010  INRIA and Microsoft Corporation
 *)

Revision.f "$Rev: 28687 $";;

(* TODO, fix the line below to work in the new build *)
(*external nprocs_internal      : unit -> int = "sysconf_nprocs"*)
let nprocs_internal m = assert false (* this is a dummy function, the above should be fixed *)

let nprocs ?(default=0) () =
  try nprocs_internal () with _ -> default
