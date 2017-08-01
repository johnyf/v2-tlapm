open Expr_ds
open Expr_visitor
open Obligation
open Backend_exceptions
open CCFormat
open Toolbox


type ppacc = term_db * op_def list * formal_param list * unit printer

let unsupported msg =
   raise (ExternalToolFailed (Commons.Isabelle, "Unsupported element", msg) )

class gen_isa_pp = object(self)
  inherit [ppacc] term_visitor as super

  method expr acc = failwith "implement!"
  method decimal acc = failwith "implement!"
  method numeral acc = failwith "implement!"
  method strng acc = failwith "implement!"
  method at acc = failwith "implement!"
  method op_appl acc = failwith "implement!"
  method binder acc = failwith "implement!"
  method lambda acc = failwith "implement!"
  method op_arg acc = failwith "implement!"
  method operator acc = failwith "implement!"
  method bound_symbol acc = failwith "implement!"
  method bounded_bound_symbol acc = failwith "implement!"
  method unbounded_bound_symbol acc = failwith "implement!"
  method formal_param acc = failwith "implement!"
  method op_decl acc = failwith "implement!"
  method op_def acc = failwith "implement!"
  method builtin_op acc = failwith "implement!"
  method user_defined_op acc = failwith "implement!"

  (* identical: expr_or_op_arg, *)
end

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
