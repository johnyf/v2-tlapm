open Commons
open Expr_ds
open Expr_visitor
open Expr_utils
open Util

open List
open Format


type nesting = Module | Expression | ProofStep
(* We need to pass on the formatter, the contect for unfolding references and a
   flag if to unfold *)
type fc = Format.formatter * context * bool * nesting

(* these are extractors for the accumulator type  *)
let ppf   (ppf, _, _, _) = ppf
let con   (_, context, _, _) = context
let undef (_, _, expand, _) = expand
let nesting (_, _, _, n) = n

(* sets the expand flag of the accumulator *)
let set_expand (x,y,_,n) v = (x,y,v,n)

(* sets the expand flag of the first accumulator the the expand flag of the
   second accumulator *)
let reset_expand (x,y,_,n) (_,_,v,_) = (x,y,v,n)

(* modifies the accumulator to turn on definition expansion *)
let enable_expand x = set_expand x true

(* modifies the accumulator to turn off definition expansion *)
let disable_expand x = set_expand x false


let set_nesting (x,y,z,_) n = (x,y,z,n)
let reset_nesting (x,y,z,_) (_,_,_,n) = (x,y,z,n)
let nest_module x = set_nesting x Module
let nest_expr x = set_nesting x Expression
let nest_proof x = set_nesting x ProofStep


let find_entry unpack acc i =
  let entries = (con acc).entries in
  let elem = assoc i entries in
  unpack elem


(* folds the function f into the given list, but extracts the formatter from
   the accumulator and prints the string s after all but the last elements.  *)
let rec ppf_fold_with ?str:(s=", ") f acc = function
  | [x] ->
     f acc x
  | x::xs ->
     let y = f acc x in
     fprintf (ppf acc) "%s" s;
     ppf_fold_with ~str:s f y xs
  | [] -> acc

(** encloses the given string with parenthesis. can be used as a %a
    argument in a formatter.  *)
let ppf_parens ppf x = fprintf ppf "(%s)" x
(** encloses the given string with a formatting box. can be used as a %a
    argument in a formatter.  *)
let ppf_box ppf x = fprintf ppf "@[%s@]" x
(** prints the given string as it is. can be used as a %a
    argument in a formatter.  *)
let ppf_ident ppf x = fprintf ppf "%s" x

(** extracts the ppf from the given accumulator and outputs a newline *)
let ppf_newline acc = fprintf (ppf acc) "@\n"

class formatter =
object(self)
  inherit [fc] visitor as super

  (* parts of expressions *)
  method location acc l : 'a = acc
  method level acc l : 'a = acc

  (* non-recursive expressions *)
  method decimal acc { location; level; mantissa; exponent;  } =
    let value =
      (float_of_int mantissa) /. ( 10.0 ** (float_of_int exponent)) in
    fprintf (ppf acc) "%s" (string_of_float value);
    acc

  method numeral acc {location; level; value } =
    fprintf (ppf acc) "%s" (string_of_int value);
    acc

  method strng acc {location; level; value} =
    fprintf (ppf acc) "%s" value;
    acc

  method op_arg acc {location; level; name; arity } =
    fprintf (ppf acc) "%s" name;
    acc

  (* recursive expressions *)
  method at acc0 {location; level; except; except_component} =
    let acc1 = self#location acc0 location in
    let acc2 = self#level acc1 level in
    let acc3 = self#op_appl acc2 except in
    let acc = self#op_appl acc3 except_component in
    (* todo make this better or manually remove the @ operators? *)
    fprintf (ppf acc) "@@" ;
    acc

  method op_appl acc0 {location; level; operator; operands; bound_symbols} =
    let acc1 = self#location acc0 location in
    let acc2 = self#level acc1 level in
    let acc3 = self#operator acc2 operator in
    let oparens, cparens = if (operands <> []) then ("(",")") else ("","") in
    fprintf (ppf acc3) "%s" oparens;
    let acc4 = ppf_fold_with self#expr_or_op_arg acc3 operands in
    fprintf (ppf acc4) "%s" cparens;
    (*     fprintf (ppf acc4) "(* bound symbols: ";
     let acc = List.fold_left self#bound_symbol acc4 bound_symbols in
     fprintf (ppf acc) " *)"; *)
    acc4

  method bounded_bound_symbol acc { params; tuple; domain; } =
    match params with
    | [] ->
       failwith "Trying to process empty tuple of bound symbols with domain!"
    | [param] ->
       if tuple then fprintf (ppf acc) "<<";
       let acc1 = ppf_fold_with self#formal_param acc params in
       if tuple then fprintf (ppf acc1) ">> \\in ";
       let acc2 = self#expr acc domain in
       acc2
    | _ ->  (*TODO: implement *)
       failwith "Implementation error: not yet implemented."

  method unbounded_bound_symbol acc { param; tuple } =
    if tuple then fprintf (ppf acc) "<<";
    let acc1 = self#formal_param acc param in
    if tuple then fprintf (ppf acc1) ">> \\in ";
    acc1

  method formal_param acc0 = function
    | FP_ref i ->
       let fp = find_entry unpack_fp_entry acc0 i in
       self#formal_param acc0 (FP fp)
    | FP { location; level; name; arity; } ->
       let acc1 = self#location acc0 location in
       let acc2 = self#level acc1 level in
       let acc3 = self#name acc2 name in
       fprintf (ppf acc3) "%s" name;
       (* arity skipped *)
       acc3

  method mule acc0 = function
    | MOD_ref i ->
       failwith "Implementation error: cannot dereference modules!"
    | MOD {name; location; constants; variables;
           definitions; assumptions; theorems; } ->
       match undef acc0 with
       | false -> (* don't expand module name *)
          fprintf (ppf acc0) "%s" name;
          acc0
       | true -> (* expand module name *)
          let acc0a = nest_module acc0 in
          fprintf (ppf acc0a) "@[<v 2>";
          fprintf (ppf acc0a) "==== %s ====@\n" name;
          (* let acc0a = self#name acc0 name in
           let acc1 = self#location acc0a location in *)
          let s_constants, s_constantsnl =
            if (constants == []) then "", "" else "CONSTANTS ", "\n" in
          fprintf (ppf acc0a) "%s" s_constants;
          let acc2 = ppf_fold_with self#op_decl acc0a constants in
          fprintf (ppf acc2) "%s" s_constantsnl;
          let s_variables, s_variablesnl =
            if (constants == []) then "", "" else "VARIABLES ", "\n" in
          fprintf (ppf acc2) "%s" s_variables;
          let acc3 = ppf_fold_with self#op_decl acc2 variables in
          fprintf (ppf acc2) "%s" s_variablesnl;
          let s_opdefnl = if (definitions == []) then "" else "\n" in
          let acc4 = ppf_fold_with ~str:"\n" self#op_def acc3 definitions in
          fprintf (ppf acc4) "%s" s_opdefnl;
          let s_assumptionsnl = if (assumptions == []) then "" else "\n" in
          let acc5 = ppf_fold_with ~str:"\n" self#assume acc4 assumptions in
          fprintf (ppf acc5) "%s" s_assumptionsnl;
          let s_theoremnl = if (theorems == []) then "" else "\n" in
          let acc6 = ppf_fold_with ~str:"\n" self#theorem acc5 theorems in
          let acc = reset_nesting acc6 acc0 in
          fprintf (ppf acc) "%s" s_theoremnl;
          fprintf (ppf acc) "@]";
          fprintf (ppf acc) "@\n------------@\n";
          acc


  method op_decl acc0 = function
    | OPD_ref x ->
       let opd = find_entry unpack_opdecl_entry acc0 x in
       self#op_decl acc0 (OPD opd)
    | OPD  { location ; level ; name ; arity ; kind ; } ->
       (* the kind is only relevant in the new_symb rule *)
       (* terminal node *)
       let acc1 = self#location acc0 location in
       let acc2 = self#level acc1 level in
       let acc3 = self#name acc2 name in
       fprintf (ppf acc3) "%s" name ;
       acc3

  method op_def acc = function
    | OPDef_ref x ->
       let op = find_entry unpack_opdef_entry acc x in
       self#op_def acc (OPDef op)
    | OPDef (O_module_instance x) ->
       self#module_instance acc x
    | OPDef (O_builtin_op x)      ->
       self#builtin_op acc x
    | OPDef (O_user_defined_op x) ->
       let op = match x with
       | UOP d -> d
       | UOP_ref r ->
          let opd = find_entry unpack_opdef_entry acc r in
          match opd with
          | O_user_defined_op (UOP op) -> op
          | _ -> failwith ("The id " ^ (string_of_int r) ^
                           " does refer to a user defined operator!")
       in
       match nesting acc, undef acc with
       | Module, _ ->
          fprintf (ppf acc) "%s == " op.name;
          let acc1 = nest_expr acc in
          let acc2 = self#user_defined_op acc1 x in
          let acc3 = reset_nesting acc2 acc in
          ppf_newline acc3;
          acc3
       | Expression, true ->
          let acc1 = nest_expr acc in
          let acc2 = self#user_defined_op acc1 x in
          let acc3 = reset_nesting acc2 acc in
          acc3
       | Expression, false ->
          fprintf (ppf acc) "%s" op.name;
          acc
       | ProofStep, _ ->
          failwith ("TODO: implement printing of op definitions in " ^
                    "proof step environments.")

  method theorem acc0 = function
    | THM_ref x ->
       let thm = find_entry unpack_thm_entry acc0 x in
       self#theorem acc0 (THM thm)
    | THM { location; level; name; expr; proof; suffices } ->
       match undef acc0, name with
       | true, _ ->
          fprintf (ppf acc0) "THEOREM ";
          let acc1 = self#location acc0 location in
          let acc2 = self#level acc1 level in
          let named = match nesting acc2, name with
          | Module, Some name -> name ^ " == "
          | _ -> ""
          in
          let s = if suffices then "SUFFICES " else "" in
          fprintf (ppf acc2) "%s %s" named s;
          let acc2a = nest_expr acc2 in
          let acc3 = self#assume_prove acc2a expr in
          let acc3a = nest_proof acc3 in
          let acc4 = self#proof acc3 proof  in
          let acc4a = reset_nesting acc4 acc0 in
          ppf_newline acc4a;
          acc4a
       | false, Some name ->
          fprintf (ppf acc0) "%s" name;
          acc0
       | _ -> failwith
                ("Implementation error! Trying to pretty print a theorem's " ^
                   "name, but the theorem does not have one.")

  method assume acc0  = function
    | ASSUME_ref x ->
       self#reference acc0 x
    | ASSUME {location; level; expr; } ->
       let acc1 = self#location acc0 location in
       let acc2 = self#level acc1 level in
       fprintf (ppf acc2) "ASSUME " ;
       let acc = self#expr acc2 expr in
       ppf_newline acc;
       acc

  method proof acc0 = function
    | P_omitted location ->
       fprintf (ppf acc0) " OMITTED";
       ppf_newline acc0;
       acc0
    | P_obvious location ->
       fprintf (ppf acc0) " OBVIOUS";
       ppf_newline acc0;
       acc0
    | P_by { location; level; facts; defs; only }  ->
       let acc1 = self#location acc0 location in
       let acc2 = self#level acc1 level in
       let by_only = if only then " BY ONLY " else " BY " in
       fprintf (ppf acc2) "%s" by_only;
       let acc3 = disable_expand acc2 in
       let acc4 = ppf_fold_with
                    self#expr_or_module_or_module_instance acc3 facts in
       let bydef = if (defs <> []) then " DEF " else "" in
       fprintf (ppf acc3) " %s" bydef;
       (* this loops because of self-reference to the containing theorem *)
       let acc5 = ppf_fold_with ~str:"\n"
                   self#defined_expr acc4 defs in
       let acc = reset_expand acc5 acc0 in
       ppf_newline acc;
       acc
    | P_steps { location; level; steps; } ->
       let acc1 = disable_expand acc0 in
       let acc2 = ppf_fold_with ~str:"" self#step acc1 steps in
       ppf_newline acc2;
       let acc = reset_expand acc2 acc1 in
       ppf_newline acc;
       acc
    | P_noproof ->
       ppf_newline acc0;
       acc0

  method step acc0 = function
     | S_def_step x -> self#def_step acc0 x
     | S_use_or_hide x -> self#use_or_hide acc0 x
     | S_instance i -> self#instance acc0 i
     | S_theorem t ->
        (* dereference theorem *)
        let thm = match t with
        | THM_ref x ->
           find_entry unpack_thm_entry acc0 x
        | THM x -> x
        in
        match thm.name with
        | Some name ->
           fprintf (ppf acc0) "%s " name;
           let acc1 = enable_expand acc0 in
           let acc2 = self#theorem acc1 t in
           let acc = reset_expand acc2 acc0 in
           (*           ppf_newline acc; *)
           acc
        | None -> failwith "A theorem as proofstep needs a name!"

  method use_or_hide acc0 {  location; level; facts; defs; only; hide } =
    let acc1 = self#location acc0 location in
    let acc2 = self#level acc1 level in
    let uoh = if hide then " HIDE " else " USE " in
    fprintf (ppf acc2) "%s" uoh;
    let acc3 = ppf_fold_with
                 self#expr_or_module_or_module_instance acc2 facts in
    let bydef = if (defs <> []) then " DEF " else "" in
    fprintf (ppf acc3) "%s" bydef;
    let acc = ppf_fold_with
                self#defined_expr acc3 defs in
    ppf_newline acc;
    acc

  method instance acc0 {location; level; name; substs; params; } =
    let acc1 = self#location acc0 location in
    let acc2 = self#level acc1 level in
    fprintf (ppf acc2) "INSTANCE ";
    let acc3 = self#name acc2 name in
    fprintf (ppf acc2) " == ";
    let acc4 = List.fold_left self#subst acc3 substs in
    let acc = List.fold_left self#formal_param acc4 params in
    ppf_newline acc;
    acc

  method subst acc0 { op; expr } =
    let acc1 = self#op_decl acc0 op in
    fprintf (ppf acc1) " <- ";
    let acc = self#expr_or_op_arg acc1 expr in
    acc

  method assume_prove acc0 { location; level; new_symbols; assumes;
                             prove; suffices; boxed; } =
    let acc1 = self#location acc0 location in
    let acc2 = self#level acc1 level in
    let s_suffices, s_prove = match (suffices, new_symbols, assumes) with
      | (true,  [], []) -> failwith "Suffices may only occur with assumptions!"
      | (false, [], []) -> "", ""
      | (false, _,  _) ->  "ASSUME ", " PROVE "
      | (true,  _,  _) -> "SUFFICES ASSUME ", " PROVE "
    in
    fprintf (ppf acc2) "%s" s_suffices;
    let acc3 = ppf_fold_with
                 self#new_symb acc2 new_symbols in
    let sep = if (new_symbols <> []) then ", " else "" in
    fprintf (ppf acc3) "%s" sep;
    let acc4 = ppf_fold_with
                 self#assume_prove acc3 assumes in
    fprintf (ppf acc2) "%s" s_prove;
    let acc = self#expr acc4 prove in
    (* ppf_newline acc; *)
    acc

  method new_symb acc0 { location; level; op_decl; set } =
    let acc1 = self#location acc0 location in
    let acc2 = self#level acc1 level in
    fprintf (ppf acc2) "NEW ";
    (* dereference op_decl *)
    let od = match op_decl with
    | OPD_ref x ->
       find_entry unpack_opdecl_entry acc0 x
    | OPD x -> x
    in
    let new_decl = match od.kind with
         | NewConstant -> "CONSTANT "
         | NewVariable -> "VARIABLE "
         | NewState -> "STATE "
         | NewAction -> "ACTION "
         | NewTemporal -> "TEMPORAL "
         | _ -> failwith "declared new symbol with a non-new kind."
    in
    let acc3 = self#op_decl acc2 op_decl in
    let acc = match set with
      | None -> acc3
      | Some e ->
         fprintf (ppf acc3) " \\in ";
         self#expr acc3 e
    in acc

  method let_in acc0 {location; level; body; op_defs } =
    let acc1 = self#location acc0 location in
    let acc2 = self#level acc1 level in
    let acc3 = self#expr acc2 body in
    let acc = List.fold_left self#op_def_or_theorem_or_assume acc3 op_defs in
    acc

  method subst_in acc0 ({ location; level; substs; body } : subst_in) =
    let acc1 = self#location acc0 location in
    let acc2 = self#level acc1 level in
    let acc3 = List.fold_left self#subst acc2 substs in
    let acc = self#expr acc3 body in
    acc

  method label acc0 ({location; level; name; arity; body; params } : label) =
    let acc1 = self#location acc0 location in
    let acc2 = self#level acc1 level in
    let acc3 = self#name acc2 name in
    (* skip arity *)
    let acc4 = self#assume_prove acc3 body in
    let acc = List.fold_left self#formal_param acc4 params in
    acc

  method ap_subst_in acc0 ({ location; level; substs; body } : ap_subst_in) =
    let acc1 = self#location acc0 location in
    let acc2 = self#level acc1 level in
    let acc3 = List.fold_left self#subst acc2 substs in
    let acc = self#node acc3 body in
    acc

  method def_step acc0 { location; level; defs } =
    let acc1 = self#location acc0 location in
    let acc2 = self#level acc1 level in
    let acc = List.fold_left self#op_def acc2 defs in
    acc

  method module_instance acc0 = function
    | MI_ref x ->
       self#reference acc0 x
    | MI {location; level; name} ->
       let acc1 = self#location acc0 location in
       let acc2 = self#level acc1 level in
       let acc = self#name acc2 name in
       acc

  method builtin_op acc0 = function
    | { level; name; arity; params } ->
       let acc1 = self#level acc0 level in
       let acc2 = self#name acc1 name in
       fprintf (ppf acc0) "%s" (self#translate_builtin_name name);
       acc2

  method user_defined_op acc0 = function
    | UOP_ref x ->
       self#reference acc0 x
    | UOP { location; level ; name ; arity ;
            body ; params ; recursive ; } ->
       match undef acc0, recursive with
       | true, false -> (* expand the definition *)
          let acc1 = self#location acc0 location in
          let acc2 = self#level acc1 level in
          let acc4 = self#expr acc2 body in
          (* TODO: handle recursive definitions *)
          (*
          let acc = List.fold_left
                    (fun x (fp,_) -> self#formal_param x fp) acc4 params in
           *)
          acc4
       | _ -> (* don't exapand the definition *)
          let acc1 = self#location acc0 location in
          let acc2 = self#level acc1 level in
          fprintf (ppf acc2) "%s" name;
          let acc3 = self#name acc2 name in
          acc3

  method name acc x = acc

  method reference acc x = acc


  method context acc { entries; modules } =
    let acc1 = List.fold_left self#mule acc modules in
    acc1

  method translate_builtin_name = function
    (*    | "$AngleAct"  as x -> failwith ("Unknown operator " ^ x ^"!") *)
    | "$BoundedChoose" -> "CHOOSE"
    | "$BoundedExists" -> "EXISTS"
    | "$BoundedForall" -> "\\A"
    | "$Case" -> "CASE"
    | "$CartesianProd" -> "\times"
    | "$ConjList" -> "/\\"
    | "$DisjList" -> "\\/"
    | "$Except" -> "EXCEPT"
    (*    | "$FcnApply" as x -> failwith ("Unknown operator " ^ x ^"!") *)
    (*    | "$FcnConstructor"  as x -> failwith ("Unknown operator " ^ x ^"!") *)
    | "$IfThenElse" -> "IFTHENELSE"
(*    | "$NonRecursiveFcnSpec"  as x -> failwith ("Unknown operator " ^ x ^"!")
    | "$Pair"  as x -> failwith ("Unknown operator " ^ x ^"!")
    | "$RcdConstructor"  as x -> failwith ("Unknown operator " ^ x ^"!")
    | "$RcdSelect"  as x -> failwith ("Unknown operator " ^ x ^"!")
    | "$RecursiveFcnSpec"  as x -> failwith ("Unknown operator " ^ x ^"!")
    | "$Seq"  as x -> failwith ("Unknown operator " ^ x ^"!")
    | "$SquareAct"  as x -> failwith ("Unknown operator " ^ x ^"!")
    | "$SetEnumerate"  as x -> failwith ("Unknown operator " ^ x ^"!")
    | "$SF"  as x -> failwith ("Unknown operator " ^ x ^"!")
    | "$SetOfAll" as x  -> failwith ("Unknown operator " ^ x ^"!")
    | "$SetOfRcds" as x  -> failwith ("Unknown operator " ^ x ^"!")
    | "$SetOfFcns" as x  -> failwith ("Unknown operator " ^ x ^"!")
    | "$SubsetOf" as x  -> failwith ("Unknown operator " ^ x ^"!")
    | "$Tuple" as x  -> failwith ("Unknown operator " ^ x ^"!") *)
    | "$TemporalExists" -> "\\EE"
    | "$TemporalForall" -> "\\AA"
    | "$UnboundedChoose" -> "CHOOSE"
    | "$UnboundedExists" -> "\\E"
    | "$UnboundedForall" -> "\\A"
(*    | "$WF" as x  -> failwith ("Unknown operator " ^ x ^"!")
    | "$Nop" as x -> failwith ("Unknown operator " ^ x ^"!") *)
    | "$Qed" -> "QED"
    | "$Pfcase" -> "CASE"
    | "$Have" -> "HAVE"
    | "$Take" -> "TAKE"
    | "$Pick" -> "PICK"
    | "$Witness" -> "WITNESS"
    | "$Suffices" -> "SUFFICES"
    | x -> x (* catchall case *)
end

let expr_formatter = new formatter
