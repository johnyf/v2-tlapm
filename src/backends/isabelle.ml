open Expr_ds
open Obligation
open Backend_exceptions
open CCFormat
open Toolbox

let encode fmt e =
  fprintf fmt "(* here comes the goal *)@,";
  ()

let debug msg =
  Format.printf "%s@." msg


let fmt_isabelle = ( fun  fmt (fn, obl) ->
    fprintf fmt "(* automatically generated *)@,";
    fprintf fmt "theory %s imports Constant Zenon begin@," fn;
    encode fmt (obl.goal);
    fprintf fmt "end";
    fprintf fmt "(* end of file *)@,";
  ) |> CCFormat.vbox


